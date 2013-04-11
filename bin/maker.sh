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

echo "Java version"
echo `java -version`
MAKER_OWN_LIB_DIR=$MAKER_OWN_ROOT_DIR/.maker/lib/
MAKER_OWN_IVY_SETTINGS_FILE=$MAKER_OWN_ROOT_DIR/ivysettings.xml
MAKER_COMPILED_PROJ_INPUT_DIR=$MAKER_PROJECT_ROOT_DIR/project-src
MAKER_OWN_SCALATEST_REPORTER_JAR=$MAKER_OWN_ROOT_DIR/maker-scalatest-reporter.jar
MAKER_OWN_SCALATEST_REPORTER_SOURCE=$MAKER_OWN_ROOT_DIR/test-reporter/src/maker/scalatest/MakerTestReporter.scala
MAKER_PROJECT_SCALA_LIB_DIR="$MAKER_PROJECT_ROOT_DIR/scala-lib/"
MAKER_PROJECT_ZINC_LIB_DIR="$MAKER_PROJECT_ROOT_DIR/zinc-libs/"

MAKER_IVY_JAR=$MAKER_OWN_ROOT_DIR/ivy-2.3.0-rc2.jar
MAKER_HEAP_SPACE=2000m
MAKER_PERM_GEN_SPACE=500m

debug(){
  msg=$1
  if [ -n "$MAKER_DEBUG" ]; then
    echo $msg
  fi
}

main() {
  process_options $*
  saveStty
  check_setup_sane || exit -1

  update_external_jars_if_required
  bootstrap_maker_if_required
  recompile_project_if_required

  if [ -z $MAKER_SKIP_LAUNCH ];
  then
    launch_maker_repl
  fi
}

project_output_dir(){
  echo `dirname $MAKER_COMPILED_PROJ_INPUT_DIR`/project-classes
}

launch_maker_repl(){
  JAVA_OPTS=" -Xmx$MAKER_HEAP_SPACE -XX:MaxPermSize=$MAKER_PERM_GEN_SPACE $JREBEL_OPTS $MAKER_DEBUG_PARAMETERS -XX:+HeapDumpOnOutOfMemoryError -XX:+UseCodeCacheFlushing -XX:ReservedCodeCacheSize=256m -XX:+CMSClassUnloadingEnabled "$JAVA_OPTS" "

  if [ ! -z $MAKER_CMD ];
  then
    CMDS="-e $MAKER_CMD"
    RUNNING_EXEC_MODE=" -Dmaker.execmode=true "
    echo "setting cmd as $CMDS"
  fi
  CLASSPATH="$(maker_internal_classpath):$(external_jars):$MAKER_OWN_ROOT_DIR/resources/:$(project_output_dir)"
  # launcher maker in the repl, with the compiled project definitions on the classpath and scripted project definition files interpreted using the -i option on scala repl
  $JAVA_HOME/bin/java $JAVA_OPTS -classpath $CLASSPATH -Dsbt.log.format="false" -Dmaker.home="$MAKER_OWN_ROOT_DIR" -Dlogback.configurationFile=$MAKER_OWN_ROOT_DIR/logback.xml $RUNNING_EXEC_MODE -Dscala.usejavacp=true $MAKER_ARGS scala.tools.nsc.MainGenericRunner -Yrepl-sync -nc -i $MAKER_PROJECT_FILE $CMDS | tee maker-session.log ; scala_exit_status=${PIPESTATUS[0]}
}

recompile_project_if_required(){
  if [ -z $MAKER_RECOMPILE_PROJECT ]; then
    echo "Skipping project compilation"
  else
    recompile_project
  fi
}

recompile_project(){
  MAKER_COMPILED_PROJ_INPUT_FILES=`ls $MAKER_COMPILED_PROJ_INPUT_DIR/*.scala | xargs`
  echo "Compiling project definitions from $MAKER_COMPILED_PROJ_INPUT_DIR directory, containing files: $MAKER_COMPILED_PROJ_INPUT_FILES ..."
  if [ -e $(project_output_dir) ];
  then
    rm -rf $(project_output_dir)
  fi
  mkdir -p $(project_output_dir) 

  # compile the maker project files in the -c specified input dir
  echo "compiling to $(project_output_dir)"
  java -classpath "$(external_jars):$(maker_internal_classpath)" -Dscala.usejavacp=true  scala.tools.nsc.Main -d $(project_output_dir) $MAKER_COMPILED_PROJ_INPUT_FILES | tee $MAKER_OWN_ROOT_DIR/proj-compile-output ; test ${PIPESTATUS[0]} -eq 0 || exit -1
}

maker_internal_classpath(){
  if [ $MAKER_DEVELOPER_MODE ];
  then
    for module in utils maker; do
      cp="$cp:$MAKER_OWN_ROOT_DIR/$module/target-maker/classes:$MAKER_OWN_ROOT_DIR/$module/target-maker/test-classes/:$MAKER_OWN_SCALATEST_REPORTER_JAR"
    done
  else
    cp="$MAKER_OWN_ROOT_DIR/maker.jar:$MAKER_OWN_SCALATEST_REPORTER_JAR"
  fi
  for module in utils maker; do
    cp="$cp:$MAKER_OWN_ROOT_DIR/$module/resources/"
  done
  echo $cp
}

check_setup_sane(){
  debug "Checking sanity"

  if [ -z $JAVA_HOME ];
  then
    echo "JAVA_HOME not defined"
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


}

run_command(){
  command="$1"
  $command || (echo "failed to run $command " && exit -1)
}

external_jars() {
  cp=`ls $MAKER_OWN_LIB_DIR/*.jar | xargs | sed 's/ /:/g'`
  echo $cp
}

build_test_reporter_jar() {
  echo "Building test reporter jar"
  rm -f $MAKER_OWN_SCALATEST_REPORTER_JAR
  TEMP_OUTPUT_DIR=`mktemp -d maker-tmp-XXXXXXXXXX`

  debug "java -classpath $(external_jars) -Dscala.usejavacp=true scala.tools.nsc.Main -d $TEMP_OUTPUT_DIR $MAKER_OWN_SCALATEST_REPORTER_SOURCE"

  java -classpath $(external_jars) -Dscala.usejavacp=true scala.tools.nsc.Main -d $TEMP_OUTPUT_DIR $MAKER_OWN_SCALATEST_REPORTER_SOURCE 2>&1 | tee $MAKER_OWN_ROOT_DIR/vim-compile-output ; test ${PIPESTATUS[0]} -eq 0 || exit 44 

  run_command "$JAVA_HOME/bin/jar cf $MAKER_OWN_SCALATEST_REPORTER_JAR -C $TEMP_OUTPUT_DIR . " || exit -1
  rm -rf $TEMP_OUTPUT_DIR
}

build_maker_jar() {
  echo "Building maker jar"
  pushd $MAKER_OWN_ROOT_DIR  # Shouldn't be necessary to change dir, but get weird compilation errors otherwise
  TEMP_OUTPUT_DIR=`mktemp -d maker-tmp-XXXXXXXXXX`
  MAKER_OWN_RESOURCES_DIR=$MAKER_OWN_ROOT_DIR/utils/resources
  MAKER_OWN_JAR=$MAKER_OWN_ROOT_DIR/maker.jar

  rm -f $MAKER_OWN_JAR

  for module in utils maker; do
    SRC_FILES="$SRC_FILES $(find $MAKER_OWN_ROOT_DIR/$module/src -name '*.scala' | xargs)"
  done

  debug "java -classpath $(external_jars) -Dscala.usejavacp=true scala.tools.nsc.Main  -d $TEMP_OUTPUT_DIR $SRC_FILES "
  java -classpath $(external_jars) -Dscala.usejavacp=true scala.tools.nsc.Main  -d $TEMP_OUTPUT_DIR $SRC_FILES 2>&1 | tee $MAKER_OWN_ROOT_DIR/vim-compile-output ; test ${PIPESTATUS[0]} -eq 0 || exit -1
  run_command "$JAVA_HOME/bin/jar cf $MAKER_OWN_JAR -C $TEMP_OUTPUT_DIR . -C $MAKER_OWN_RESOURCES_DIR ." || exit -1

  if [ ! -e $MAKER_OWN_ROOT_DIR/maker.jar ];
  then
    echo "Maker jar failed to be created"
    exit -1
  fi

  rm -rf $TEMP_OUTPUT_DIR

  popd
}

bootstrap_maker_if_required() {
  if [ ! -z $MAKER_BOOTSTRAP ]; then
    build_maker_jar || exit -1
    build_test_reporter_jar || exit -1
  fi
}

process_options() {
  debug "Processing options"

  while true; do
    case "${1-""}" in
      -h | --help ) display_usage; exit 0;;
      -r | --revision ) MAKER_BINARY_VERSION=$2; shift 2;;
      -p | -i | --project-file ) MAKER_PROJECT_FILE=$2; shift 2;;
      -c | --project-input-dir ) MAKER_COMPILED_PROJ_INPUT_DIR=$2; shift 2;;
      -d | --clean-project-class-files) MAKER_RECOMPILE_PROJECT=true; shift 1;;
      -e | --exec-cmd ) MAKER_CMD=$2; shift 2;;
      -j | --use-jrebel ) set_jrebel_options; shift;;
      -m | --mem-heap-space ) MAKER_HEAP_SPACE=$2; shift 2;;
      -y | --do-ivy-update ) MAKER_IVY_UPDATE=true; shift;;
      -b | --boostrap ) MAKER_BOOTSTRAP=true; shift;;
      -x | --allow-remote-debugging ) MAKER_DEBUG_PARAMETERS="-Xdebug -Xrunjdwp:transport=dt_socket,server=y,suspend=n,address=5005"; shift;;
      -z | --developer-mode ) MAKER_DEVELOPER_MODE=true; shift;;
      -nr | --no-repl ) MAKER_SKIP_LAUNCH=true; shift 1;;
      -ntty | --no-tty-restore ) echo; echo "DEPRECATED OPTION '-ntty', THIS CAN BE REMOVED"; echo; shift 1;;
      -args | --additional-args ) shift 1; MAKER_ARGS=$*; break;;
      --mem-permgen-space ) MAKER_PERM_GEN_SPACE=$2; shift 2;;
      --ivy-proxy-host ) MAKER_IVY_PROXY_HOST=$2; shift 2;;
      --ivy-proxy-port ) MAKER_IVY_PROXY_PORT=$2; shift 2;;
      --ivy-non-proxy-hosts ) MAKER_IVY_NON_PROXY_HOSTS=$2; shift 2;; 
      --ivy-url ) MAKER_IVY_URL=$2; shift 2;;
      --compiler-interface-url ) MAKER_COMPILER_INTERFACE_URL=$2; shift 2;;
      --ivy-settings-file ) MAKER_OWN_IVY_SETTINGS_FILE=$2; shift 2;;
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
    -c, --project-input-dir <directory> a directory containing Scala file(s) for a compiled project definition
      compile Scala in directory before loading Maker in REPL
    -d | --clean-project-class-files) 
      clean any compiled project scala files
    -e, --exec-cmd
      run command directly then quit
    -j, --use-jrebel (requires JREBEL_HOME to be set)
    -m, --mem-heap-space <heap space in MB> 
      default is one quarter of available RAM
    -y, --do-ivy-update 
      update will always be done if <maker-dir>/.maker/lib doesn't exist
    -b, --boostrap 
      builds maker.jar from scratch
    -x, --allow-remote-debugging
      runs a remote JVM
    -z, --developer-mode
      For maker development
      Sets the maker classpath to maker/classes:utils/classes etc rather than 
      maker.jar. Allows work on maker and another project to be done simultaneously.
    -nr, --no-repl
      skip repl launch (just performs bootstrapping/building and returns)
    --args, --additional-args
      additional variable length argument list to pass to JVM process directly. Must come at the end of the arguments
    --mem-permgen-space <space in MB>
      default is 1/10th of heap space
    --ivy-proxy-host <host>
    --ivy-proxy-port <port>
    --ivy-non-proxy-hosts <host,host,...>
    --ivy-jar <file>
      defaults to /usr/share/java/ivy.jar
    --ivy-url <url>        
      For bootstrapping. Defaults to http://repo1.maven.org/maven2/org/apache/ivy/ivy/2.3.0-rc2/ivy-2.3.0-rc2.jar
    --compiler-interface-url <url>
      Zinc has a dependency on this. Defaults to http://maker.googlecode.com/files/compiler-interface-sources-0.12.1.jar
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
  command="$command -settings $MAKER_OWN_IVY_SETTINGS_FILE "
  command="$command -retrieve $lib_dir/[artifact]-[revision](-[classifier]).[ext] "
  echo $command
}


update_external_jars_if_required(){
  if [ $MAKER_IVY_UPDATE ]; 
  then
    fetch_bootstrap_jar ${MAKER_IVY_URL:="http://repo1.maven.org/maven2/org/apache/ivy/ivy/2.3.0-rc2/ivy-2.3.0-rc2.jar"} $MAKER_OWN_ROOT_DIR || exit -1

    run_command "$(ivy_command $MAKER_OWN_ROOT_DIR/ivy-files/scala-compiler-ivy.xml $MAKER_OWN_LIB_DIR) -types jar sync" || exit -1
    run_command "$(ivy_command $MAKER_OWN_ROOT_DIR/ivy-files/scala-library-ivy.xml $MAKER_OWN_LIB_DIR) -types jar" || exit -1
    run_command "$(ivy_command $MAKER_OWN_ROOT_DIR/utils/ivy.xml $MAKER_OWN_LIB_DIR) -types jar" || exit -1

    run_command "$(ivy_command $MAKER_OWN_ROOT_DIR/ivy-files/scala-library-ivy.xml $MAKER_PROJECT_SCALA_LIB_DIR) -types jar sync" || exit -1
    run_command "$(ivy_command $MAKER_OWN_ROOT_DIR/ivy-files/scala-library-ivy.xml $MAKER_PROJECT_SCALA_LIB_DIR) -types source" || exit -1
    run_command "$(ivy_command $MAKER_OWN_ROOT_DIR/ivy-files/scala-compiler-ivy.xml $MAKER_PROJECT_ZINC_LIB_DIR) -types jar sync" || exit -1

    fetch_bootstrap_jar ${MAKER_COMPILER_INTERFACE_URL:="http://maker.googlecode.com/files/compiler-interface-sources-0.12.1.jar"} $MAKER_PROJECT_ZINC_LIB_DIR || exit -1
  fi
}


fetch_bootstrap_jar(){
  url=$1
  lib_dir=$2
  jar_file=`basename $url`
  echo "Fetching $jar_file"
  if [ ! -z $MAKER_IVY_PROXY_HOST ]; then
    CURL_PROXY_ARGS="-x http://$MAKER_IVY_PROXY_HOST:$MAKER_IVY_PROXY_PORT"
  fi
  debug "downloading Ivy jar from $url - $CURL_PROXY_ARGS"
  echo "curl $CURL_PROXY_ARRGS -O $url"
  curl $CURL_PROXY_ARRGS -O $url
  if [ ! -e $lib_dir ];
  then
    mkdir -p $lib_dir
  fi
  mv $jar_file $lib_dir
  if [ ! -f ${lib_dir}/${jar_file} ]; then
      echo "Failed to download $jar_file"
      exit -1
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
function onExit() {
  if [[ "$saved_stty" != "" ]]; then
    stty $saved_stty
    saved_stty=""
  #else
    # I think we always want these on, regardless of whether
    # we are called interactively
    #stty "echo icanon"
  fi
  exit $scala_exit_status
}

# to reenable echo if we are interrupted before completing.
trap onExit INT SIGTERM EXIT

# save terminal settings
function saveStty() {
  debug "Saving stty"
  if tty -s; then
    saved_stty=$(stty -g 2>/dev/null)
  else
    saved_stty=""
  fi
}

scala_exit_status=127
main $*
onExit
