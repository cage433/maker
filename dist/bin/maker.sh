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
MAKER_OWN_LIB_DIR=$MAKER_OWN_ROOT_DIR/.maker/lib
MAKER_PROJECT_SCALA_LIB_DIR=.maker/scala-lib
MAKER_IVY_SETTINGS_FILE=ivysettings.xml
MAKER_COMPILED_PROJ_OUTPUT_DIR=$MAKER_OWN_ROOT_DIR/.maker/proj

mkdir -p .maker

main() {
  process_options $*
  saveStty
  check_setup_sane || exit -1

  if [ $MAKER_IVY_UPDATE ] || [ ! -e $MAKER_OWN_LIB_DIR ];
  then
    ivy_update
  else
    echo "Omitting ivy update as $MAKER_OWN_LIB_DIR exists"
  fi
  
  if [ $MAKER_BOOTSTRAP ] || [ ! -e $MAKER_OWN_ROOT_DIR/maker.jar ];
  then
    bootstrap || exit -1
  else
    echo "Omitting bootstrap as $MAKER_OWN_ROOT_DIR/maker.jar exists"
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
      echo "setting cmd as $CMDS"
    fi

    # pre-compile the project definition file and add to the classpath
    if [ ! -z $COMPILE_PROJECT ];
    then
      # are we already up to date?
      if test $MAKER_COMPILED_PROJ_OUTPUT_DIR -nt $MAKER_PROJECT_FILE ; then
        echo "Skipping project compilation, already up to date"
      else
        echo "Compiling project definition $MAKER_PROJECT_FILE"
        if [ -e $MAKER_COMPILED_PROJ_OUTPUT_DIR ]; then
          rm -rf $MAKER_COMPILED_PROJ_OUTPUT_DIR 
        fi
        mkdir $MAKER_COMPILED_PROJ_OUTPUT_DIR
	# compile the maker project file
        $SCALA_HOME/bin/scalac -classpath "$(external_jars):$CLASSPATH" -d $MAKER_COMPILED_PROJ_OUTPUT_DIR $MAKER_PROJECT_FILE | tee $MAKER_OWN_ROOT_DIR/proj-compile-output ; test ${PIPESTATUS[0]} -eq 0 || exit -1
      fi
      # launcher maker with the compiled project file on the classpath...
      $JAVA_HOME/bin/java -Xbootclasspath/a:$(scala_jars) -classpath "$CLASSPATH:$MAKER_COMPILED_PROJ_OUTPUT_DIR" $JAVA_OPTS -Dmaker.home="$MAKER_OWN_ROOT_DIR" -Dmaker.level="0" -Dmaker.process.hierarchy="repl" -Dscala.usejavacp=true $MAKER_ARGS scala.tools.nsc.MainGenericRunner -Yrepl-sync -nc $CMDS | tee maker-session.log ; test ${PIPESTATUS[0]} -eq 0 || exit -1
      scala_exit_status=$?
    else
      # launcher maker in the repl, with the project definition file interpreted using the -i option on scala
      $JAVA_HOME/bin/java -Xbootclasspath/a:$(scala_jars) -classpath $CLASSPATH $JAVA_OPTS -Dmaker.home="$MAKER_OWN_ROOT_DIR" -Dmaker.process.hierarchy="repl" -Dmaker.level="0" -Dscala.usejavacp=true $MAKER_ARGS scala.tools.nsc.MainGenericRunner -Yrepl-sync -nc -i $MAKER_PROJECT_FILE $CMDS | tee maker-session.log ; test ${PIPESTATUS[0]} -eq 0 || exit -1
      scala_exit_status=$?
    fi
  fi
}

maker_internal_classpath(){
  if [ $MAKER_DEVELOPER_MODE ];
  then
    for module in utils plugin maker; do
      cp="$cp:$MAKER_OWN_ROOT_DIR/$module/target/classes:$MAKER_OWN_ROOT_DIR/$module/target/test-classes/"
    done
  else
    cp=$MAKER_OWN_ROOT_DIR/maker.jar
  fi
  for module in utils plugin maker; do
    cp="$cp:$MAKER_OWN_ROOT_DIR/$module/resources/"
  done
  echo $cp
}

check_setup_sane(){
  if [ -z $SCALA_HOME ];
  then
    echo "SCALA_HOME not defined"
    exit -1
  fi

  if [ -z $JAVA_HOME ];
  then
    echo "JAVA_HOME not defined"
    exit -1
  fi

  MAKER_IVY_JAR=${MAKER_IVY_JAR-${MAKER_OWN_ROOT_DIR}/libs/ivy-2.2.0.jar}
  if [ ! -e $MAKER_IVY_JAR ];
  then
    echo "Ivy jar not found"
    exit -1
  fi

  if [ -z $MAKER_PROJECT_FILE ];
  then
    echo "No project file defined, searching for a maker Scala file"
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
  #cp=$cp:`ls $MAKER_OWN_ROOT_DIR/.maker/scala-lib/*.jar | xargs | sed 's/ /:/g'`
  cp=$cp:`ls $MAKER_OWN_ROOT_DIR/libs/*.jar | xargs | sed 's/ /:/g'`
  echo $cp
}

scala_jars(){
  jars=`ls $SCALA_HOME/lib/*.jar | xargs | sed 's/ /:/g'`
  echo $jars
}

bootstrap() {

  pushd $MAKER_OWN_ROOT_DIR  # Shouldn't be necessary to change dir, but get weird compilation errors otherwise
  MAKER_OWN_CLASS_OUTPUT_DIR=$MAKER_OWN_ROOT_DIR/out
  MAKER_OWN_RESOURCES_DIR=$MAKER_OWN_ROOT_DIR/utils/resources
  MAKER_OWN_JAR=$MAKER_OWN_ROOT_DIR/maker.jar
  MAKER_OWN_SCALATEST_REPORTER_JAR=$MAKER_OWN_ROOT_DIR/maker-scalatest-reporter.jar

  rm -rf $MAKER_OWN_CLASS_OUTPUT_DIR
  mkdir $MAKER_OWN_CLASS_OUTPUT_DIR
  rm -f $MAKER_OWN_JAR
  rm -f $MAKER_OWN_SCALATEST_REPORTER_JAR

  # First build jar with just test reporter
  SRC_FILES="$(find $MAKER_OWN_ROOT_DIR/scalatest/src -name '*.scala' | xargs)"
  echo "Compiling test reporter"
  $SCALA_HOME/bin/scalac -classpath $(external_jars) -d $MAKER_OWN_CLASS_OUTPUT_DIR $SRC_FILES | tee $MAKER_OWN_ROOT_DIR/vim-compile-output ; test ${PIPESTATUS[0]} -eq 0 || exit -1
  echo "Building test reporter jar"
  run_command "$JAVA_HOME/bin/jar cf $MAKER_OWN_SCALATEST_REPORTER_JAR -C $MAKER_OWN_CLASS_OUTPUT_DIR . " || exit -1

  for module in utils plugin maker; do
    SRC_FILES="$SRC_FILES $(find $MAKER_OWN_ROOT_DIR/$module/src -name '*.scala' | xargs)"
  done

  echo "Compiling"
  $SCALA_HOME/bin/scalac -classpath $(external_jars) -d $MAKER_OWN_CLASS_OUTPUT_DIR $SRC_FILES | tee $MAKER_OWN_ROOT_DIR/vim-compile-output ; test ${PIPESTATUS[0]} -eq 0 || exit -1
  echo "Building jar"
  run_command "$JAVA_HOME/bin/jar cf $MAKER_OWN_JAR -C $MAKER_OWN_CLASS_OUTPUT_DIR . -C $MAKER_OWN_RESOURCES_DIR ." || exit -1

  if [ ! -e $MAKER_OWN_ROOT_DIR/maker.jar ];
  then
	  echo "Maker jar failed to be created"
	  exit -1
  fi

  rm -rf $MAKER_OWN_CLASS_OUTPUT_DIR

  popd

}

process_options() {

  while true; do
    case "${1-""}" in
      -h | --help ) display_usage; exit 0;;
      -p | --project-file ) MAKER_PROJECT_FILE=$2; shift 2;;
      -c | --cmd ) MAKER_CMD=$2; shift 2;;
      -j | --use-jrebel ) set_jrebel_options; shift;;
      -m | --mem-heap-space ) MAKER_HEAP_SPACE=$2; shift 2;;
      -y | --do-ivy-update ) MAKER_IVY_UPDATE=true; shift;;
      -b | --boostrap ) MAKER_BOOTSTRAP=true; shift;;
      -x | --allow-remote-debugging ) MAKER_DEBUG_PARAMETERS="-Xdebug -Xrunjdwp:transport=dt_socket,server=y,suspend=n,address=5005"; shift;;
      -i | --developer-mode ) MAKER_DEVELOPER_MODE=true; shift;;
      -nr | --no-repl ) MAKER_SKIP_LAUNCH=true; shift 1;;
      -ntty | --no-tty-restore ) MAKER_NO_TTY_RESTORE=true; shift 1;;
      -args | --additional-args ) shift 1; MAKER_ARGS=$*; break;;
      --mem-permgen-space ) MAKER_PERM_GEN_SPACE=$2; shift 2;;
      --ivy-proxy-host ) MAKER_IVY_PROXY_HOST=$2; shift 2;;
      --ivy-proxy-port ) MAKER_IVY_PROXY_PORT=$2; shift 2;;
      --ivy-non-proxy-hosts ) MAKER_IVY_NON_PROXY_HOSTS=$2; shift 2;; 
      --ivy-jar ) MAKER_IVY_JAR=$2; shift 2;;
      --ivy-settings-file ) MAKER_IVY_SETTINGS_FILE=$2; shift 2;;
      -cpl | --compile-project ) COMPILE_PROJECT=true; shift 1;;
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
    -p, --project-file <project-file>
    -c, --cmd
      run command directly then quit
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
    -i, --developer-mode
      For maker developers.
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
      defaults to /usr/share/java/ivy.jar
    --ivy-settings-file <file>
      override the default ivysettings.xml file
    --compile-project
      compile project file before loading
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
  command="$command -jar $MAKER_IVY_JAR -ivy $ivy_file"
  command="$command -settings $MAKER_OWN_ROOT_DIR/$MAKER_IVY_SETTINGS_FILE "
  command="$command -retrieve $lib_dir/[artifact]-[revision](-[classifier]).[ext] "
  echo $command
}


ivy_update() {
  echo "Updating ivy"
  MAKER_IVY_FILE="$MAKER_OWN_ROOT_DIR/utils/ivy.xml"
  run_command "$(ivy_command $MAKER_IVY_FILE $MAKER_OWN_LIB_DIR) -types jar -sync"
  run_command "$(ivy_command $MAKER_IVY_FILE $MAKER_OWN_LIB_DIR) -types bundle"
  run_command "$(ivy_command $MAKER_IVY_FILE $MAKER_OWN_LIB_DIR) -types source "
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
    echo "restoring tty"
    restoreSttySettings
  fi
  echo "returning exit status: " $scala_exit_status
  exit $scala_exit_status
}

# to reenable echo if we are interrupted before completing.
trap onExit INT

# save terminal settings
function saveStty() {
  if [ -z $MAKER_NO_TTY_RESTORE ]; then
    echo "saving current tty for restore on exit"
    saved_stty=$(stty -g 2>/dev/null)
  else
    echo "skipping tty save/restore"
  fi
}

# clear on error so we don't later try to restore them
if [[ ! $? ]]; then  
  saved_stty=""
fi

scala_exit_status=127
main $*
onExit
