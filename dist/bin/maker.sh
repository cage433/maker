#!/bin/bash

# This script can be invoked using a relative path from the cwd, the path to
#   the project file will then also be relative to the cwd
#
# Simple usage:
#   ./a/b/c/maker/bin/maker.sh [-y] [-b] -p ./a/b/myproj/myprojbuild.scala
#
# This project may or may not be maker itself. To avoid further confusion, the
# following convention is used to distinguish maker and project variables.
#
# MAKER_OWN_...         refer to maker itself
# MAKER_PROJECT_...     refer to the project
#
# The script does the following
# 1. Download via ivy the jars required by maker itself 
# 2. Build maker.jar
# 3. Set classpath and heap space
# 4. Launch the repl, loading the project
# 
# Steps 1 and 2 are omitted if they have been done earlier - unless overridden in
# in the options.
# 

MAKER_OWN_ROOT_DIR="$( cd "$(dirname $( dirname "${BASH_SOURCE[0]}" ))" && pwd )"
MAKER_PROJECT_ROOT_DIR=`pwd`

set -e

#MAKER_IVY_UPDATE=true
#MAKER_BOOTSTRAP=true 
MAKER_SCALA_VERSION="2.9.1"
MAKER_OWN_LIB_DIR=$MAKER_OWN_ROOT_DIR/.maker/lib
MAKER_SCALA_DIR="$MAKER_OWN_ROOT_DIR/scala-$MAKER_SCALA_VERSION/"
MAKER_SCALA_LIB_DIR="$MAKER_SCALA_DIR/lib"
MAKER_SCALA_IVY_FILE="$MAKER_SCALA_DIR/ivy.xml"
MAKER_PROJECT_SCALA_LIB_DIR=.maker/scala-lib
MAKER_IVY_SETTINGS_FILE=ivysettings.xml
MAKER_COMPILED_PROJ_OUTPUT_DIR=$MAKER_PROJECT_ROOT_DIR/.maker/proj
MAKER_OWN_SCALATEST_REPORTER_JAR=$MAKER_OWN_ROOT_DIR/maker-scalatest-reporter.jar
MAKER_OWN_SCALATEST_REPORTER_SOURCE=$MAKER_OWN_ROOT_DIR/test-reporter/src/maker/scalatest/MakerTestReporter.scala

mkdir -p .maker/proj

main() {
  process_options $*
  saveStty
  check_setup_sane || exit -1

  write_ivy_files

  if [ $MAKER_IVY_UPDATE ] || [ ! -e $MAKER_OWN_LIB_DIR ];
  then
    ivy_update
  else
    echo "Omitting ivy update as $MAKER_OWN_LIB_DIR exists"
  fi

  if [ $MAKER_IVY_UPDATE ] || [ ! -e $MAKER_SCALA_LIB_DIR ];
  then
    ivy_scala_update
  else
    echo "Omitting ivy scala update as $MAKER_SCALA_LIB_DIR exists"
  fi

  # binary or source based bootstrapping?
  if [ $MAKER_BINARY_VERSION ]; then
    echo "Fetching binary distribution $MAKER_BINARY_VERSION"
    ivy_update
    maker_ivy_binary_retrieve
  else
    if [[ ! -z $MAKER_BOOTSTRAP ]] || [[ ! -e $MAKER_OWN_ROOT_DIR/maker.jar ]] ;
    then
      bootstrap || exit -1
    else
      echo "Omitting bootstrap as $MAKER_OWN_ROOT_DIR/maker.jar exists"
    fi

    if test $MAKER_OWN_SCALATEST_REPORTER_SOURCE -nt $MAKER_OWN_SCALATEST_REPORTER_JAR;
    then
      build_test_reporter_jar || exit -1
    fi
  fi

  if [ -z $MAKER_SKIP_LAUNCH ];
  then
    JAVA_OPTS="-Xmx$(($MAKER_HEAP_SPACE))m -XX:MaxPermSize=$(($MAKER_PERM_GEN_SPACE))m $JREBEL_OPTS $MAKER_DEBUG_PARAMETERS -XX:+HeapDumpOnOutOfMemoryError "
    # TODO - move scala jars from bootclasspath to classpath once permgen fix available
    CLASSPATH="$(maker_internal_classpath):$(external_jars):$MAKER_OWN_ROOT_DIR/resources/"
#    echo "CLASSPATH = $CLASSPATH"
#    echo "Args = $MAKER_ARGS"

    if [ ! -z $MAKER_CMD ];
    then
      CMDS="-e $MAKER_CMD"
      RUNNING_EXEC_MODE=" -Dmaker.execmode=true "
      echo "setting cmd as $CMDS"
    fi

    # check for -c compiled input dir, if provided pre-compile the project definition file and add to the classpath
    if [ ! -z $MAKER_COMPILED_PROJ_INPUT_DIR ];
    then
      MAKER_COMPILED_PROJ_INPUT_FILES=`ls $MAKER_COMPILED_PROJ_INPUT_DIR/*.scala | xargs`
      #echo "debug: Project compilation requested for files in $MAKER_COMPILED_PROJ_INPUT_DIR - found $MAKER_COMPILED_PROJ_INPUT_FILES"

      # are we already up to date?
      rmdir --ignore-fail-on-non-empty $MAKER_COMPILED_PROJ_OUTPUT_DIR #in case directory is there but no classes
      if test -e $MAKER_COMPILED_PROJ_OUTPUT_DIR  && test $MAKER_COMPILED_PROJ_OUTPUT_DIR -nt $MAKER_COMPILED_PROJ_INPUT_DIR  ; then
        echo "Skipping project compilation, already up to date "$MAKER_COMPILED_PROJ_OUTPUT_DIR
      else
        echo "Compiling project definitions from $MAKER_COMPILED_PROJ_INPUT_DIR directory, containing files: $MAKER_COMPILED_PROJ_INPUT_FILES ..."
        if [ -e $MAKER_COMPILED_PROJ_OUTPUT_DIR ]; then
          rm -rf $MAKER_COMPILED_PROJ_OUTPUT_DIR 
        fi
        mkdir $MAKER_COMPILED_PROJ_OUTPUT_DIR

	    # compile the maker project files in the -c specified input dir
	    echo "compiling to $MAKER_COMPILED_PROJ_OUTPUT_DIR"
        $JAVA_HOME/bin/java -Dscala.usejavacp=true -classpath "$(external_jars):$CLASSPATH" scala.tools.nsc.Main -d $MAKER_COMPILED_PROJ_OUTPUT_DIR $MAKER_COMPILED_PROJ_INPUT_FILES | tee $MAKER_OWN_ROOT_DIR/proj-compile-output ; test ${PIPESTATUS[0]} -eq 0 || exit -1
      fi

      # append in compiled project classes to the classpath
      CLASSPATH="$CLASSPATH:$MAKER_COMPILED_PROJ_OUTPUT_DIR"
    fi

    # launcher maker in the repl, with the compiled project definitions on the classpath and scripted project definition files interpreted using the -i option on scala repl
    $JAVA_HOME/bin/java -Xbootclasspath/a:$(scala_jars) -classpath $CLASSPATH $JAVA_OPTS -Dmaker.home="$MAKER_OWN_ROOT_DIR" -Dmaker.process.hierarchy="repl" $RUNNING_EXEC_MODE -Dmaker.level="0" -Dmaker.scala.lib.dir=$MAKER_SCALA_LIB_DIR -Dscala.usejavacp=true $MAKER_ARGS scala.tools.nsc.MainGenericRunner -Yrepl-sync -nc -i $MAKER_PROJECT_FILE $CMDS | tee maker-session.log ; scala_exit_status=${PIPESTATUS[0]}
  fi
}

maker_internal_classpath(){
  if [ $MAKER_DEVELOPER_MODE ];
  then
    for module in utils plugin maker; do
      cp="$cp:$MAKER_OWN_ROOT_DIR/$module/target/classes:$MAKER_OWN_ROOT_DIR/$module/target/test-classes/:$MAKER_OWN_SCALATEST_REPORTER_JAR"
    done
  else
    cp="$MAKER_OWN_ROOT_DIR/maker.jar:$MAKER_OWN_SCALATEST_REPORTER_JAR"
  fi
  for module in utils plugin maker; do
    cp="$cp:$MAKER_OWN_ROOT_DIR/$module/resources/"
  done
  echo $cp
}

check_setup_sane(){
  if [ -z $JAVA_HOME ];
  then
    echo "JAVA_HOME not defined"
    exit -1
  fi

  fetch_ivy

  MAKER_IVY_JAR=${MAKER_IVY_JAR-${MAKER_OWN_ROOT_DIR}/libs/ivy-2.2.0.jar}
  if [ ! -e $MAKER_IVY_JAR ];
  then
    echo "Ivy jar not found"
    exit -1
  fi

  if [ -z $MAKER_PROJECT_FILE ];
  then
    declare -a arr
    i=0
    for file in `ls *.scala`; do
      arr[$i]=$file
      ((i++))
    done
    if [ ${#arr[@]} != 1 ];
    then
      echo "Either specify project file or have a single Scala file in the top level"
      exit -1
    fi
    MAKER_PROJECT_FILE="${arr[0]}"
    echo "Assuming $MAKER_PROJECT_FILE is the project file"
  fi


  MAKER_HEAP_SPACE=${MAKER_HEAP_SPACE-$(calc_heap_space)}
  MAKER_PERM_GEN_SPACE=${MAKER_PERM_GEN_SPACE-$(($MAKER_HEAP_SPACE / 5))}
}

calc_heap_space(){
  os=${OSTYPE//[0-9.]/}
  if [ "$os" = "darwin" ];
  then
    totalMem=$(sysctl hw.memsize | awk '/[:s]/ {print $2}')
    totalMem=$(($totalMem/1024))
  else
    totalMem=$(cat /proc/meminfo | head -n 1 | awk '/[0-9]/ {print $2}')
  fi
  echo "$(($totalMem/1024/4))"
}

run_command(){
  command="$1"
  $command || (echo "failed to run $command " && exit -1)
}

external_jars() {
  cp=`ls $MAKER_OWN_ROOT_DIR/.maker/lib/*.jar | xargs | sed 's/ /:/g'`
  cp=$cp:`ls $MAKER_SCALA_LIB_DIR/*.jar | xargs | sed 's/ /:/g'`
  #cp=$cp:`ls $MAKER_OWN_ROOT_DIR/.maker/scala-lib/*.jar | xargs | sed 's/ /:/g'`
  cp=$cp:`ls $MAKER_OWN_ROOT_DIR/libs/*.jar | xargs | sed 's/ /:/g'`
  echo $cp
}

scala_jars(){
  jars=`ls $MAKER_SCALA_LIB_DIR/*.jar | xargs | sed 's/ /:/g'`
  echo $jars
}

build_test_reporter_jar() {
    echo "Building test reporter jar as source is newer"
    rm -f $MAKER_OWN_SCALATEST_REPORTER_JAR
    TEMP_OUTPUT_DIR=`mktemp -d maker-tmp-XXXXXXXXXX`

    $JAVA_HOME/bin/java -Dscala.usejavacp=true -classpath $(external_jars) scala.tools.nsc.Main -d $TEMP_OUTPUT_DIR $MAKER_OWN_SCALATEST_REPORTER_SOURCE 2>&1 | tee $MAKER_OWN_ROOT_DIR/vim-compile-output ; test ${PIPESTATUS[0]} -eq 0 || exit 44 

    run_command "$JAVA_HOME/bin/jar cf $MAKER_OWN_SCALATEST_REPORTER_JAR -C $TEMP_OUTPUT_DIR . " || exit -1
    rm -rf $TEMP_OUTPUT_DIR
}

bootstrap() {

  pushd $MAKER_OWN_ROOT_DIR  # Shouldn't be necessary to change dir, but get weird compilation errors otherwise
  TEMP_OUTPUT_DIR=`mktemp -d maker-tmp-XXXXXXXXXX`
  MAKER_OWN_RESOURCES_DIR=$MAKER_OWN_ROOT_DIR/utils/resources
  MAKER_OWN_JAR=$MAKER_OWN_ROOT_DIR/maker.jar

  rm -f $MAKER_OWN_JAR

  for module in utils plugin maker; do
    SRC_FILES="$SRC_FILES $(find $MAKER_OWN_ROOT_DIR/$module/src -name '*.scala' | xargs)"
  done

  echo "Building maker.jar"
  $JAVA_HOME/bin/java -Dscala.usejavacp=true -classpath $(external_jars) scala.tools.nsc.Main -d $TEMP_OUTPUT_DIR $SRC_FILES 2>&1 | tee $MAKER_OWN_ROOT_DIR/vim-compile-output ; test ${PIPESTATUS[0]} -eq 0 || exit -1
  run_command "$JAVA_HOME/bin/jar cf $MAKER_OWN_JAR -C $TEMP_OUTPUT_DIR . -C $MAKER_OWN_RESOURCES_DIR ." || exit -1

  if [ ! -e $MAKER_OWN_ROOT_DIR/maker.jar ];
  then
	  echo "Maker jar failed to be created"
	  exit -1
  fi

  rm -rf $TEMP_OUTPUT_DIR

  popd

}

process_options() {

  while true; do
    case "${1-""}" in
      -h | --help ) display_usage; exit 0;;
      -r | --revision ) MAKER_BINARY_VERSION=$2; shift 2;;
      -p | -i | --project-file ) MAKER_PROJECT_FILE=$2; shift 2;;
      -c | --compile-project ) MAKER_COMPILED_PROJ_INPUT_DIR=$2; shift 2;;
      -e | --exec-cmd ) MAKER_CMD=$2; shift 2;;
      -j | --use-jrebel ) set_jrebel_options; shift;;
      -m | --mem-heap-space ) MAKER_HEAP_SPACE=$2; shift 2;;
      -y | --do-ivy-update ) MAKER_IVY_UPDATE=true; shift;;
      -b | --boostrap ) MAKER_BOOTSTRAP=true; shift;;
      -x | --allow-remote-debugging ) MAKER_DEBUG_PARAMETERS="-Xdebug -Xrunjdwp:transport=dt_socket,server=y,suspend=n,address=5005"; shift;;
      -z | --developer-mode ) MAKER_DEVELOPER_MODE=true; shift;;
      -nr | --no-repl ) MAKER_SKIP_LAUNCH=true; shift 1;;
      -ntty | --no-tty-restore ) MAKER_NO_TTY_RESTORE=true; shift 1;;
      -args | --additional-args ) shift 1; MAKER_ARGS=$*; break;;
      --mem-permgen-space ) MAKER_PERM_GEN_SPACE=$2; shift 2;;
      --ivy-proxy-host ) MAKER_IVY_PROXY_HOST=$2; shift 2;;
      --ivy-proxy-port ) MAKER_IVY_PROXY_PORT=$2; shift 2;;
      --ivy-non-proxy-hosts ) MAKER_IVY_NON_PROXY_HOSTS=$2; shift 2;; 
      --ivy-jar ) MAKER_IVY_JAR=$2; shift 2;;
      --ivy-url ) MAKER_IVY_URL=$2; shift 2;;
      --ivy-settings-file ) MAKER_IVY_SETTINGS_FILE=$2; shift 2;;
      -- ) shift; break;;
      *  ) break;;
    esac
  done

  REMAINING_ARGS=$*
}

display_usage() {
cat << EOF

  usage
    maker.sh <option>*

  options
    -h, --help
    -r, --revision <version> Maker binary version number
      download binary revision and boot
    -p, -i, --include-project-file <project-file script> a scala script to load into the repl
    -e, --exec-cmd
      run command directly then quit
    -c, --compile-project <directory> a directory containing Scala file(s) for a compiled project definition
      compile Scala in directory before loading Maker in REPL
    -j, --use-jrebel (requires JREBEL_HOME to be set)
    -m, --mem-heap-space <heap space in MB> 
      default is one quarter of available RAM
    -y, --do-ivy-update 
      update will always be done if <maker-dir>/.maker/lib doesn't exist
    -b, --boostrap 
      builds maker.jar from scratch
    -d, --download-project-scala-lib 
      downloads scala compiler and library to <project-dir>/.maker/scala-lib
      download is automatic if this directory does not exist
    -x, --allow-remote-debugging
      runs a remote JVM
    -z, --developer-mode
      For maker development
      Sets the maker classpath to maker/classes:utils/classes etc rather than 
      maker.jar. Allows work on maker and another project to be done simultaneously.
    -nr, --no-repl
      skip repl launch (just performs bootstrapping/building and returns)
    -ntty, --no-tty-restore
      skip save and restore tty (for integration with automation such as TeamCity reporting)
    --args, --additional-args
      additional variable length argument list to pass to JVM process directly. Must come at the end of the arguments
    --mem-permgen-space <space in MB>
      default is 1/10th of heap space
    --ivy-proxy-host <host>
    --ivy-proxy-port <port>
    --ivy-non-proxy-hosts <host,host,...>
    --ivy-jar <file>
    --ivy-url <url>        
      defaults to /usr/share/java/ivy.jar
    --ivy-settings-file <file>
      override the default ivysettings.xml file

EOF
}


ivy_command(){
  ivy_file=$1
  lib_dir=$2
  if [ ! -e $lib_dir ];
  then
    mkdir -p $lib_dir
  fi
  command="java "
  if [ ! -z $MAKER_IVY_PROXY_HOST ];
  then
    command="$command -Dhttp.proxyHost=$MAKER_IVY_PROXY_HOST"
  fi
  if [ ! -z $MAKER_IVY_PROXY_PORT ];
  then
    command="$command -Dhttp.proxyPort=$MAKER_IVY_PROXY_PORT"
  fi
  if [ ! -z $MAKER_IVY_NON_PROXY_HOSTS ];
  then
    command="$command -Dhttp.nonProxyHosts=$MAKER_IVY_NON_PROXY_HOSTS"
  fi
  command="$command -Dmaker_binary_version=$MAKER_BINARY_VERSION -jar $MAKER_IVY_JAR -ivy $ivy_file"
  command="$command -settings $MAKER_OWN_ROOT_DIR/$MAKER_IVY_SETTINGS_FILE "
  command="$command -retrieve $lib_dir/[artifact]-[revision](-[classifier]).[ext] "
  echo $command
}


ivy_update() {
  echo "Updating dependencies (using Ivy)"
  MAKER_IVY_FILE="$MAKER_OWN_ROOT_DIR/utils/ivy.xml"
  run_command "$(ivy_command $MAKER_IVY_FILE $MAKER_OWN_LIB_DIR) -types jar -sync"
  run_command "$(ivy_command $MAKER_IVY_FILE $MAKER_OWN_LIB_DIR) -types bundle"
  run_command "$(ivy_command $MAKER_IVY_FILE $MAKER_OWN_LIB_DIR) -types source"
}

ivy_scala_update() {
  echo "Updating scala libs (using Ivy)"
  run_command "$(ivy_command $MAKER_SCALA_IVY_FILE $MAKER_SCALA_LIB_DIR) -types jar -sync"
  run_command "$(ivy_command $MAKER_SCALA_IVY_FILE $MAKER_SCALA_LIB_DIR) -types bundle"
  run_command "$(ivy_command $MAKER_SCALA_IVY_FILE $MAKER_SCALA_LIB_DIR) -types source"
}

maker_ivy_binary_retrieve() {
  echo "Fetching maker binary dependencies (using Ivy) for version ${MAKER_BINARY_VERSION}"
  MAKER_IVY_FILE="$MAKER_OWN_ROOT_DIR/maker-ivy.xml"
  run_command "$(ivy_command $MAKER_IVY_FILE $MAKER_OWN_LIB_DIR) -types jar"
  #run_command "$(ivy_command $MAKER_IVY_FILE $MAKER_OWN_LIB_DIR) -types bundle"
  #run_command "$(ivy_command $MAKER_IVY_FILE $MAKER_OWN_LIB_DIR) -types source"
}

fetch_ivy() {
  if [ -z $MAKER_IVY_URL ]; then
    MAKER_IVY_URL="http://repo1.maven.org/maven2/org/apache/ivy/ivy/2.2.0/ivy-2.2.0.jar"
  fi
  if [ ! -f ${MAKER_OWN_ROOT_DIR}/libs/ivy-2.2.0.jar ]; then
    if [ ! -z $MAKER_IVY_PROXY_HOST ]; then
      CURL_PROXY_ARGS="-x http://$MAKER_IVY_PROXY_HOST:$MAKER_IVY_PROXY_PORT"
    fi
    echo "downloading Ivy 2.2 jar from $MAKER_IVY_URL - $CURL_PROXY_ARGS"
    curl $CURL_PROXY_ARRGS -O $MAKER_IVY_URL
    mkdir -p ${MAKER_OWN_ROOT_DIR}/libs
    mv ivy-2.2.0.jar ${MAKER_OWN_ROOT_DIR}/libs/
  fi
}

set_jrebel_options() {
  if [ ! -f $JREBEL_HOME/jrebel.jar ];
  then
    echo "Can't find jrebel.jar, set JREBEL_HOME"
    exit 1
  fi
  JREBEL_OPTS=" -javaagent:$JREBEL_HOME/jrebel.jar -noverify"
}


# restore stty settings (echo in particular)
function restoreSttySettings() {
  stty $saved_stty
  saved_stty=""
}

function onExit() {
  if [[ "$saved_stty" != "" ]]; then
    restoreSttySettings
  fi
  exit $scala_exit_status
}

# to reenable echo if we are interrupted before completing.
trap onExit INT SIGTERM EXIT

# save terminal settings
function saveStty() {
  if [ -z $MAKER_NO_TTY_RESTORE ]; then
    saved_stty=$(stty -g 2>/dev/null)
  else
    echo "skipping tty save/restore"
  fi
}

# clear on error so we don't later try to restore them
if [[ ! $? ]]; then  
  saved_stty=""
fi

#write out the embedded ivy files for Maker to bootstrap its dependencies via Ivy
function write_ivy_files() {

mkdir -p ${MAKER_OWN_ROOT_DIR}/utils/
mkdir -p $MAKER_SCALA_DIR
mkdir -p ${MAKER_OWN_ROOT_DIR}/test-reporter/

cat > $MAKER_SCALA_IVY_FILE<<SCALA_LIBS_IVY_FILE
<!-- Auto-generated from Maker script -->
<ivy-module version="1.0" xmlns:e="http://ant.apache.org/ivy/extra">
  <info organisation="\${group_id}" module="utils" revision="\${maker.module.version}" />
  <configurations>
    <conf name="default" transitive="false"/>
    <conf name="compile" transitive="false"/>
    <conf name="test" transitive="false"/>
  </configurations>

  <dependencies defaultconfmapping="*->default,sources">
    <dependency org="org.scala-lang" name="scala-library" rev="$MAKER_SCALA_VERSION"/>
    <dependency org="org.scala-lang" name="scala-compiler" rev="$MAKER_SCALA_VERSION"/>
    <dependency org="org.scala-lang" name="jline" rev="$MAKER_SCALA_VERSION"/>
  </dependencies>
</ivy-module>
SCALA_LIBS_IVY_FILE

cat > ${MAKER_OWN_ROOT_DIR}/utils/ivy.xml<<IVY_FILE
<!-- Auto-generated from Maker script -->
<ivy-module version="1.0" xmlns:e="http://ant.apache.org/ivy/extra">
  <info organisation="\${group_id}" module="utils" revision="\${maker.module.version}" />
  <configurations>
    <conf name="default" transitive="false"/>
    <conf name="compile" transitive="false"/>
    <conf name="test" transitive="false"/>
  </configurations>

  <publications>
    <artifact name="utils" type="pom"/>
    <artifact name="utils" type="jar" ext="jar" conf="default" />
    <artifact name="utils-sources" type="jar" ext="jar" conf="default" />
    <artifact name="utils-docs" type="jar" ext="jar" conf="default" />
  </publications>

  <dependencies defaultconfmapping="*->default,sources">
    <dependency org="log4j" name="log4j" rev="1.2.16" />
    <dependency org="commons-io" name="commons-io" rev="2.1"/>
    <dependency org="commons-codec" name="commons-codec" rev="1.6"/>
    <dependency org="org.apache.commons" name="commons-lang3" rev="3.1"/>
    <dependency org="org.scala-tools.testing" name="scalacheck_$MAKER_SCALA_VERSION" rev="1.9"/>
    <dependency org="org.scalatest" name="scalatest_$MAKER_SCALA_VERSION" rev="\${scalatest_version}"/>
    <dependency org="org.scalaz" name="scalaz-core_$MAKER_SCALA_VERSION" rev="6.0.4"/>
    <dependency org="org.slf4j" name="slf4j-api" rev="1.6.1"/>
    <dependency org="org.slf4j" name="slf4j-log4j12" rev="1.6.1" />
    <dependency org="org.apache.ant" name="ant" rev="1.8.2"/>
    <dependency org="io.netty" name="netty" rev="3.5.2.Final"/>
    <dependency org="com.google.protobuf" name="protobuf-java" rev="2.4.1"/>
    <dependency org="net.debasishg" name="sjson_$MAKER_SCALA_VERSION" rev="0.15"/>
    <dependency org="org.eclipse.jetty" name="jetty-server" rev="\${jetty_version}" />
    <dependency org="org.eclipse.jetty" name="jetty-webapp" rev="\${jetty_version}" />
    <dependency org="org.eclipse.jetty" name="jetty-util" rev="\${jetty_version}" />
    <dependency org="org.eclipse.jetty" name="jetty-servlet" rev="\${jetty_version}" />
    <dependency org="org.eclipse.jetty" name="jetty-security" rev="\${jetty_version}" />
    <dependency org="org.eclipse.jetty" name="jetty-http" rev="\${jetty_version}" />
    <dependency org="org.eclipse.jetty" name="jetty-io" rev="\${jetty_version}" />
    <dependency org="org.eclipse.jetty" name="jetty-xml" rev="\${jetty_version}" />
    <dependency org="org.eclipse.jetty" name="jetty-continuation" rev="\${jetty_version}" />
    <dependency org="org.eclipse.jetty" name="jetty-jsp" rev="\${jetty_version}" />

    <dependency org="org.mortbay.jetty" name="jsp-2.1-glassfish" rev="2.1.v20100127" />
    <dependency org="javax.servlet" name="servlet-api" rev="2.5" />
    <dependency org="org.apache.tomcat" name="jsp-api" rev="6.0.20" />
    <dependency org="org.mockito" name="mockito-all" rev="1.8.2" />
    <dependency org="org.apache.ivy" name="ivy" rev="2.2.0" />
  </dependencies>
</ivy-module>
IVY_FILE

cat > ${MAKER_OWN_ROOT_DIR}/test-reporter/ivy.xml<<TEST_REPORTER_IVY_FILE
<!-- Auto-generated from Maker script -->
<ivy-module version="1.0" xmlns:e="http://ant.apache.org/ivy/extra">
  <info organisation="\${group_id}" module="test-reporter" revision="\${maker.module.version}" />
  <configurations>
    <conf name="default" transitive="false"/>
    <conf name="compile" transitive="false"/>
    <conf name="test" transitive="false"/>
  </configurations>

  <publications>
    <artifact name="test-reporter" type="pom"/>
    <artifact name="test-reporter" type="jar" ext="jar" conf="default" />
    <artifact name="test-reporter-sources" type="jar" ext="jar" conf="default" />
    <artifact name="test-reporter-docs" type="jar" ext="jar" conf="default" />
  </publications>

  <dependencies defaultconfmapping="*->default,sources">
    <dependency org="org.scalatest" name="scalatest_$MAKER_SCALA_VERSION" rev="\${scalatest_version}"/>
  </dependencies>
</ivy-module>
TEST_REPORTER_IVY_FILE

cat > ${MAKER_OWN_ROOT_DIR}/maker-ivy.xml <<MAKER_IVY_FILE
<!-- Auto-generated from Maker script -->
<ivy-module version="1.0" xmlns:e="http://ant.apache.org/ivy/extra">
  <info organisation="com.google.code" module="maker-downloader" />
  <configurations>
    <conf name="default" transitive="false"/>
  </configurations>
  <dependencies defaultconfmapping="*->default">
    <dependency org="com.google.code.maker" name="maker" rev="\${maker_binary_version}" />
    <dependency org="com.google.code.maker" name="plugin" rev="\${maker_binary_version}" />
    <dependency org="com.google.code.maker" name="utils" rev="\${maker_binary_version}" />
    <dependency org="com.google.code.maker" name="test-reporter" rev="\${maker_binary_version}" />
  </dependencies>
</ivy-module>
MAKER_IVY_FILE

cat > ${MAKER_OWN_ROOT_DIR}/ivysettings.xml <<IVY_SETTINGS
<!-- Auto-generated from Maker script -->
<ivysettings>
  <property name="group_id" value="com.google.code.maker" />
  <property name="maker.module.version" value="0.01" />
  <property name="scala_version" value="$MAKER_SCALA_VERSION" />
  <property name="scalatest_version" value="1.8" />
  <property name="ivy.local.default.root" value="\${ivy.default.ivy.user.dir}/maker-local" override="false"/>
  <property name="jetty_version" value="7.6.3.v20120416" />
  <settings>
    <settings name="default" transitive="false"/>
  </settings>
  <settings defaultResolver="default"/>
  <!-- not sure how to get params from maker into this yet... 
  <credentials host="oss.sonatype.org" realm="Sonatype Nexus Repository Manager" username="\${maker.ivy.publish.username}" passwd="\${maker.ivy.publish.password}" />
 -->
  <credentials host="oss.sonatype.org" realm="Sonatype Nexus Repository Manager" username="LouisB" passwd="x"/>

  <resolvers>
    <filesystem name="maker-local" m2compatible="true">
      <artifact pattern="\${ivy.local.default.root}/maker-local/[module]/[revision]/[artifact]-[revision].[ext]" />
    </filesystem>
    <url name="maker-oss-snapshot" m2compatible="true" > <!-- Sonatype OSS Snapshots -->
      <!-- <artifact pattern="https://oss.sonatype.org/content/repositories/snapshots/com/google/code/maker/maker-test/[revision]/[artifact]/[artifact]-[revision].[ext]" /> -->
      <artifact pattern="https://oss.sonatype.org/content/repositories/snapshots/[organisation]/test/[revision]/[artifact]/[artifact]-[revision].[ext]" />
    </url>
    <url name="maker-oss-staging" m2compatible="true"> <!-- Sonatype OSS Staging -->
      <artifact pattern="https://oss.sonatype.org/service/local/staging/deploy/maven2/maker-test/[artifact]/[revision]/[artifact]/pom.xml]" />
    </url>
    <ibiblio name="central" m2compatible="true"/>
    <ibiblio name="maker" m2compatible="true" root="https://oss.sonatype.org/content/repositories/snapshots/" />
    <chain name="default" returnFirst="true">
      <resolver ref="central"/>
      <resolver ref="maker-oss-snapshot"/>
    </chain>
  </resolvers>
</ivysettings>
IVY_SETTINGS

}

scala_exit_status=127
main $*
onExit
