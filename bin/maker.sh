#!/bin/bash

# The project maker is managing may or may not be maker itself. To avoid confusion
# variables are prefixed with MAKER_ or PROJECT_. 
#
# The script does the following
# 1. Downloads third party jars required by maker itself 
# 2. Builds maker.jar and maker-scalatest-reporter.jar
# 3. Compiles any project definition scala files other than the file the repl loads
# 4. Set classpath and heap space
# 5. Launch the repl, loading the project definition file
# 
# Steps 1 to 3 are omitted if they have been done earlier - unless overridden in
# in the options. The project definition file is the single scala file in PWD, unless
# a file is passed in as a parameter

if [ -e ".javahome" ]; then
  export JAVA_HOME=`cat .javahome`
  export PATH=$JAVA_HOME/bin:$PATH
fi

if [ "$OSTYPE" = "msys" ]
then
  PSEP=';'
  FIXCP="sed -e s/^\/\([A-Za-z]\)\//\1:\//;s/\([\x20;]\)\/\([A-Za-z]\)\//\1\2:\//g"
else
  PSEP=':'
  FIXCP='cat -'
fi

MAKER_ROOT_DIR="$( cd "$(dirname $( dirname "${BASH_SOURCE[0]}" ))" && pwd )"
PROJECT_ROOT_DIR=`pwd`

MAKER_JAR=$MAKER_ROOT_DIR/.maker/maker-libs/maker.jar

MAKER_HEAP_SPACE=4000m
MAKER_PERM_GEN_SPACE=1000m
GLOBAL_RESOURCE_CACHE=$HOME/.maker-resource-cache

mkdir -p .maker
rm -f .maker/maker-shell-errors
source $MAKER_ROOT_DIR/bin/src/utils.sh

check_for_errors(){
  if has_error; then
    cat .maker/maker-shell-errors
    exit -1
  fi
}
main() {
  if [ -z "$JAVA_HOME" ];
  then
    echo "JAVA_HOME not defined"
    exit -1
  fi
  MAKER_OPTIONS=$*
  echo "maker; processing options: $MAKER_OPTIONS"
  process_options $MAKER_OPTIONS
  saveStty

  update_logback_test_config
  update_external_jars && check_for_errors
  recompile_project_if_required && check_for_errors

  launch_maker_repl
}

update_logback_test_config(){
  if [ ! -e $PROJECT_ROOT_DIR/.maker/logback-unit-tests.xml ]; then
    cat > $PROJECT_ROOT_DIR/.maker/logback-unit-tests.xml <<HERE
<!-- The logback file used during the running of maker's own unit tests
     Quietens the logging somewhat-->

<configuration scan="true" scanPeriod="3 seconds">

  <appender name="FILE" class="ch.qos.logback.core.FileAppender">
    <file>maker.log</file>
    <append>true</append>
    <encoder>
      <pattern>%d{HH:mm:ss.SSS} [%thread] %-5level - %msg%n</pattern>
      <immediateFlush>true</immediateFlush>
    </encoder>
    <filter class="ch.qos.logback.classic.filter.ThresholdFilter">
      <level>INFO</level>
    </filter>
  </appender>
        
  <appender name="CONSOLE" class="ch.qos.logback.core.ConsoleAppender">
    <encoder>
      <pattern>%d{HH:mm:ss.SSS} [%thread] %-5level - %msg%n</pattern>
      <immediateFlush>true</immediateFlush>
    </encoder>
    <filter class="ch.qos.logback.classic.filter.ThresholdFilter">
      <level>ERROR</level>
    </filter>
  </appender>

  <root level="info">
    <appender-ref ref="CONSOLE" />
    <appender-ref ref="FILE" />
  </root>
</configuration>
HERE
  fi
  
}
update_external_jars(){
  echo "Updating external jars"

  mkdir -p $GLOBAL_RESOURCE_CACHE
  GLOBAL_RESOURCE_CONFIG="$MAKER_ROOT_DIR/external-resource-config" 

  for dir in "test-reporter" "utils"; do 
    update_resources $MAKER_ROOT_DIR/$dir/lib_managed $MAKER_ROOT_DIR/$dir/external-resources
  done
  

  MAKER_SCALA_VERSION="2.10.4"
  cat > maker-scala-resource-list <<HERE
org.scala-lang scala-library $MAKER_SCALA_VERSION 
org.scala-lang jline $MAKER_SCALA_VERSION 
org.scala-lang scala-compiler $MAKER_SCALA_VERSION 
org.scala-lang scala-reflect $MAKER_SCALA_VERSION 
HERE
  update_resources $MAKER_ROOT_DIR/.maker/repl-libs maker-scala-resource-list 
  rm maker-scala-resource-list

#  # TODO - add version to maker jars
#  cat > maker-jar-list << HERE
#com.github.cage433 maker-test-reporter 0.03 path:maker-test-reporter.jar
#com.github.cage433 maker 0.03 path:maker.jar
#HERE
#  update_resources $MAKER_ROOT_DIR/.maker/maker-libs maker-jar-list 
#  rm maker-jar-list

  cat > dynamic-scala-resource-list <<HERE
org.scala-lang scala-library {scala_version} classifier:sources 
org.scala-lang scala-library {scala_version} 
org.scala-lang scala-compiler {scala_version} 
org.scala-lang scala-compiler {scala_version} classifier:sources 
org.scala-lang scala-reflect {scala_version} 
org.scala-lang scala-reflect {scala_version} classifier:sources 
HERE

  update_resources $PROJECT_ROOT_DIR/.maker/scala-libs dynamic-scala-resource-list  
  rm dynamic-scala-resource-list

}


launch_maker_repl(){
  if [ ! -z $MAKER_CMD ];
  then
    CMDS="-e $MAKER_CMD"
    RUNNING_EXEC_MODE=" -Dmaker.execmode=true "
    echo "setting cmd as $CMDS"
  fi
 
  JAVA_OPTS=" -Xmx$MAKER_HEAP_SPACE \
    -XX:MaxPermSize=$MAKER_PERM_GEN_SPACE \
    $MAKER_JAVA_OPTS \
    -XX:+HeapDumpOnOutOfMemoryError \
    -XX:+UseCodeCacheFlushing \
    -XX:ReservedCodeCacheSize=256m \
    -XX:+CMSClassUnloadingEnabled \
    $JAVA_OPTS"

  if [ -z $PROJECT_FILE ];
  then
    scala_files=( `ls *.scala ` )
    if [ ${#scala_files[@]} -ne 1 ]; then
      echo
      echo "Error!"
      echo "Either specify project file or have a single Scala file in the top level"
      echo
      exit -1
    fi
    PROJECT_FILE=${scala_files[0]}
  fi

  "$JAVA_HOME/bin/java" $JAVA_OPTS \
    -classpath $(scala_jars) \
    -Dsbt.log.format="false" \
    $RUNNING_EXEC_MODE \
    -Dlogback.configurationFile=$MAKER_ROOT_DIR/logback-config/logback.xml \
    -Dscala.usejavacp=true \
    $MAKER_ARGS \
    $EXTRA_REPL_ARGS \
    scala.tools.nsc.MainGenericRunner \
    -cp "$(maker_classpath)${PSEP}$PROJECT_DEFINITION_CLASS_DIR" \
    -Yrepl-sync -nc \
    -i $PROJECT_FILE \
    $CMDS \
    | tee maker-session.log ; scala_exit_status=${PIPESTATUS[0]}
}

recompile_project_if_required(){

  if [ -e $PROJECT_DEFINITION_SRC_DIR ] && ([ ! -e $PROJECT_DEFINITION_CLASS_DIR ] || \
     has_newer_src_files $PROJECT_DEFINITION_SRC_DIR $PROJECT_DEFINITION_CLASS_DIR || \
     [ ! -z $MAKER_RECOMPILE_PROJECT ]); 
  then
    echo "Recompiling project"
    PROJECT_DEFINITION_SRC_FILES=`ls $PROJECT_DEFINITION_SRC_DIR/*.scala | xargs`
    echo "Compiling $PROJECT_DEFINITION_SRC_FILES"
    rm -rf $PROJECT_DEFINITION_CLASS_DIR
    mkdir -p $PROJECT_DEFINITION_CLASS_DIR

    echo "java -classpath $(scala_jars)${PSEP}$(maker_classpath) -Dscala.usejavacp=true  scala.tools.nsc.Main -d $PROJECT_DEFINITION_CLASS_DIR $PROJECT_DEFINITION_SRC_FILES"
    java -classpath "$(scala_jars)${PSEP}$(maker_classpath)" -Dscala.usejavacp=true  scala.tools.nsc.Main -d $PROJECT_DEFINITION_CLASS_DIR $PROJECT_DEFINITION_SRC_FILES || exit -1
  fi
}

maker_classpath(){
  cp=$(ls "$MAKER_ROOT_DIR"/utils/lib_managed/*.jar | xargs | sed 's/ /'${PSEP}'/g' | $FIXCP)
  if [ $MAKER_DEVELOPER_MODE ];
  then
    for module in utils maker; do
      cp="$cp${PSEP}$MAKER_ROOT_DIR/$module/target-maker/classes${PSEP}$MAKER_ROOT_DIR/$module/target-maker/test-classes/"
    done
  else
    cp="$cp${PSEP}$MAKER_JAR"
  fi
  echo $cp | $FIXCP
}


run_command(){
  eval "$1" || (echo "failed to run $1 " && exit -1)
}
scala_jars() {
  ls "$MAKER_ROOT_DIR"/.maker/repl-libs/*.jar \
    | xargs | sed 's/ /'${PSEP}'/g' | $FIXCP
}


process_options() {

  while true; do
    case "${1-""}" in
      -h | --help ) display_usage; exit 0;;
      -p | -i | --project-file ) PROJECT_FILE=$2; shift 2;;
      -c | --project-definition-src ) PROJECT_DEFINITION_SRC_DIR=$2; shift 2;;
      -e | --exec-cmd ) MAKER_CMD=$2; shift 2;;
      -d | --clean-project-class-files) MAKER_RECOMPILE_PROJECT=true; shift 1;;
      -m | --mem-heap-space ) MAKER_HEAP_SPACE=$2; shift 2;;
      -y | --do-ivy-update ) MAKER_IVY_UPDATE=true; shift;;
      -x | --allow-remote-debugging ) MAKER_JAVA_OPTS="$MAKER_JAVA_OPTS -Xdebug -Xrunjdwp:transport=dt_socket,server=y,suspend=n,address=5005"; shift;;
      -z | --developer-mode ) MAKER_DEVELOPER_MODE=true; shift;;
      -j | --use-jrebel ) MAKER_JAVA_OPTS="$MAKER_JAVA_OPTS -javaagent:/usr/local/jrebel/jrebel.jar "; shift 1;;
      --mem-permgen-space ) MAKER_PERM_GEN_SPACE=$2; shift 2;;
      -args | -- ) shift; EXTRA_REPL_ARGS=$*; break;;
      *  ) break;;
    esac
  done
  PROJECT_DEFINITION_SRC_DIR=${PROJECT_DEFINITION_SRC_DIR-$PROJECT_ROOT_DIR/project-src}
  PROJECT_DEFINITION_CLASS_DIR=`dirname $PROJECT_DEFINITION_SRC_DIR`/project-classes
}

display_usage() {
cat << EOF

  usage
    maker.sh <option>*

  options
    -h, --help

    -p, -i, --include-project-file <project-file script> 
      scala script to load into the repl. 
      default is unique .scala file in PWD if it exists

    -c, --project-definition-src <directory> 
      a directory containing scala file(s) used by the include file
      for performance these are compiled

    -e, --exec-cmd <CMD>
      run command directly then quit

    -d | --clean-project-class-files
      DEPRECATED
      Recompile project definition source files. This should happen automatically

    -m, --mem-heap-space <heap space in MB> 
      default is one quarter of available RAM

    -y, --update-external-jars
      DEPRECATED
      Download external jars and any other resources. Should happen automaticaly

    -x, --allow-remote-debugging
      runs a remote JVM

    -z, --developer-mode
      For maker development
      Sets the maker classpath to maker/classes${PSEP}utils/classes etc rather than 
      maker.jar. Allows work on maker and another project to be done simultaneously.

    --mem-permgen-space <space in MB>
      default is 1/10th of heap space

    -args, -- <any>* 
      all subsequent arguments passed directly to repl

EOF
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
  saved_stty=""
  if test -x /usr/bin/tty; then
    if tty -s; then
      saved_stty=$(stty -g 2>/dev/null)
    fi
  fi
}

# this won't work if the terminal is resized
export MAKER_TERM_LINES=`tput lines`

scala_exit_status=127
main $*
onExit
