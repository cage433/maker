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

MAKER_ROOT_DIR="$( cd "$(dirname $( dirname "${BASH_SOURCE[0]}" ))" && pwd )"
PROJECT_ROOT_DIR=`pwd`

echo $MAKER_ROOT_DIR
MAKER_JAR=$MAKER_ROOT_DIR/maker.jar
MAKER_SCALATEST_REPORTER_JAR=$MAKER_ROOT_DIR/maker-scalatest-reporter.jar

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
  if [ -z $JAVA_HOME ];
  then
    echo "JAVA_HOME not defined"
    exit -1
  fi
  process_options $*
  saveStty

  update_external_jars && check_for_errors
  bootstrap_maker_if_required && check_for_errors
  recompile_project_if_required && check_for_errors

  launch_maker_repl
}

update_external_jars(){
  echo "Updating external jars"

  mkdir -p $GLOBAL_RESOURCE_CACHE
  GLOBAL_RESOURCE_RESOLVERS="$MAKER_ROOT_DIR/resource-resolvers" 
  GLOBAL_RESOURCE_VERSIONS="$MAKER_ROOT_DIR/resource-versions" 

  for dir in "test-reporter" "utils"; do 
    update_resources $MAKER_ROOT_DIR/$dir/lib_managed $MAKER_ROOT_DIR/$dir/external-resources
  done
  

  cat > dynamic-zinc-resource-list <<HERE
com.typesafe.sbt compiler-interface 0.12.1 classifier:sources 
com.typesafe.sbt incremental-compiler {sbt_version} 
com.typesafe.sbt sbt-interface {sbt_version}
HERE
  update_resources $MAKER_ROOT_DIR/zinc-libs dynamic-zinc-resource-list 

  cat > dynamic-scala-resource-list <<HERE
org.scala-lang scala-library {scala_version} classifier:sources path:scala-library-{scala_version}-sources.jar 
org.scala-lang scala-library {scala_version} path:scala-library-{scala_version}.jar
org.scala-lang scala-compiler {scala_version} path:scala-compiler-{scala_version}.jar
org.scala-lang scala-compiler {scala_version} classifier:sources path:scala-compiler-sources-{scala_version}.jar
HERE
  update_resources $MAKER_ROOT_DIR/scala-libs dynamic-scala-resource-list 

  GLOBAL_RESOURCE_RESOLVERS="$PROJECT_ROOT_DIR/resource-resolvers" 
  GLOBAL_RESOURCE_VERSIONS="$PROJECT_ROOT_DIR/resource-versions" 
  update_resources $PROJECT_ROOT_DIR/scala-libs dynamic-scala-resource-list  
  rm dynamic-scala-resource-list
  rm dynamic-zinc-resource-list
}

build_jar(){
  read jar_name src_files <<<$(echo $*)

  # Not sure why it's necessary to go to the maker root directory. Get 
  # strange compilation errors otherwise
  pushd $MAKER_ROOT_DIR

  echo "Building $jar_name"
  rm -f $jar_name
  TEMP_OUTPUT_DIR=`mktemp -d maker-tmp-XXXXXXXXXX`


  java -classpath $(external_jars) \
    -Dscala.usejavacp=true \
    scala.tools.nsc.Main \
    -d $TEMP_OUTPUT_DIR \
    $src_files 2>&1 \
    | tee $MAKER_ROOT_DIR/vim-compile-output ; test ${PIPESTATUS[0]} -eq 0 || exit -1

  run_command "$JAVA_HOME/bin/jar cf $jar_name -C $TEMP_OUTPUT_DIR . " || exit -1
  rm -rf $TEMP_OUTPUT_DIR
  popd
}

bootstrap_maker_if_required() {
  if [ ! -e $MAKER_JAR ]  || \
     [ ! -e $MAKER_SCALATEST_REPORTER_JAR ] || \
     has_newer_src_files $MAKER_ROOT_DIR/maker/src $MAKER_JAR || \
     has_newer_src_files $MAKER_ROOT_DIR/utils/src $MAKER_JAR || \
     has_newer_src_files $MAKER_ROOT_DIR/test-reporter/src $MAKER_SCALATEST_REPORTER_JAR;
  then
    echo "Building maker"
    build_jar $MAKER_SCALATEST_REPORTER_JAR "$MAKER_ROOT_DIR/test-reporter/src/maker/scalatest/MakerTestReporter.scala"

    for module in utils maker; do
      SRC_FILES="$SRC_FILES $(find $MAKER_ROOT_DIR/$module/src -name '*.scala' | xargs)"
    done
    build_jar $MAKER_JAR $SRC_FILES

    MAKER_RECOMPILE_PROJECT=true
  fi
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
    $MAKER_DEBUG_PARAMETERS \
    -XX:+HeapDumpOnOutOfMemoryError \
    -XX:+UseCodeCacheFlushing \
    -XX:ReservedCodeCacheSize=256m \
    -XX:+CMSClassUnloadingEnabled \
    $JAVA_OPTS"

  if [ -z $PROJECT_FILE ];
  then
    scala_files=( `ls *.scala` )
    if [ ${#scala_files[@]} -ne 1 ]; then
      echo "Either specify project file or have a single Scala file in the top level"
      exit -1
    fi
    PROJECT_FILE=${scala_files[0]}
  fi

  # echo "Maker script DEBUG: $EXTRA_REPL_ARGS"

  JAVA_COMMAND_ARGS=" $JAVA_OPTS \
    -classpath "$(maker_classpath):$PROJECT_DEFINITION_CLASS_DIR" \
    -Dsbt.log.format="false" \
    -Dmaker.home="$MAKER_ROOT_DIR" \
    $RUNNING_EXEC_MODE \
    -Dlogback.configurationFile=$MAKER_ROOT_DIR/logback.xml \
    -Dscala.usejavacp=true \
    $MAKER_ARGS \
    $EXTRA_REPL_ARGS \
    scala.tools.nsc.MainGenericRunner \
    -Yrepl-sync -nc \
    -i $PROJECT_FILE \
    $CMDS \
    | tee maker-session.log ; scala_exit_status=${PIPESTATUS[0]}"

  # echo "DEBUG: JAVA COMMAND = $JAVA_COMMAND"
  
  $JAVA_HOME/bin/java $JAVA_COMMAND_ARGS
}

recompile_project_if_required(){
  PROJECT_DEFINITION_SRC_DIR=${PROJECT_DEFINITION_SRC_DIR-$PROJECT_ROOT_DIR/project-src}
  PROJECT_DEFINITION_CLASS_DIR=`dirname $PROJECT_DEFINITION_SRC_DIR`/project-classes

  if [ ! -e $PROJECT_DEFINITION_CLASS_DIR ] || \
     has_newer_src_files $PROJECT_DEFINITION_SRC_DIR $PROJECT_DEFINITION_CLASS_DIR || \
     [ ! -z $MAKER_RECOMPILE_PROJECT ]; 
  then
    echo "Recompiling project"
    PROJECT_DEFINITION_SRC_FILES=`ls $PROJECT_DEFINITION_SRC_DIR/*.scala | xargs`
    echo "Compiling $PROJECT_DEFINITION_SRC_FILES"
    rm -rf $PROJECT_DEFINITION_CLASS_DIR
    mkdir -p $PROJECT_DEFINITION_CLASS_DIR

    java -classpath "$(maker_classpath)" -Dscala.usejavacp=true  scala.tools.nsc.Main -d $PROJECT_DEFINITION_CLASS_DIR $PROJECT_DEFINITION_SRC_FILES || exit -1
  fi
}

maker_classpath(){
  cp=$(external_jars)
  if [ $MAKER_DEVELOPER_MODE ];
  then
    for module in utils maker test-reporter; do
      cp="$cp:$MAKER_ROOT_DIR/$module/target-maker/classes:$MAKER_ROOT_DIR/$module/target-maker/test-classes/"
    done
  else
    cp="$cp:$MAKER_JAR:$MAKER_SCALATEST_REPORTER_JAR"
  fi
  echo $cp
}


run_command(){
  command="$1"
  $command || (echo "failed to run $command " && exit -1)
}

external_jars() {
  ls $MAKER_ROOT_DIR/utils/lib_managed/*.jar \
     $MAKER_ROOT_DIR/test-reporter/lib_managed/*.jar \
     $MAKER_ROOT_DIR/scala-libs/*.jar \
    | xargs | sed 's/ /:/g'
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
      -b | --boostrap ) MAKER_BOOTSTRAP=true; shift;;
      -x | --allow-remote-debugging ) MAKER_DEBUG_PARAMETERS="-Xdebug -Xrunjdwp:transport=dt_socket,server=y,suspend=n,address=5005"; shift;;
      -z | --developer-mode ) MAKER_DEVELOPER_MODE=true; shift;;
      --mem-permgen-space ) MAKER_PERM_GEN_SPACE=$2; shift 2;;
     -args | -- ) shift; EXTRA_REPL_ARGS=$*; break;;
      *  ) break;;
    esac
  done
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

    -b, --boostrap 
      DEPRECATED
      Builds maker.jar from scratch. This should happen automatically if the
      jar is older than any maker source file.

    -x, --allow-remote-debugging
      runs a remote JVM

    -z, --developer-mode
      For maker development
      Sets the maker classpath to maker/classes:utils/classes etc rather than 
      maker.jar. Allows work on maker and another project to be done simultaneously.

    --mem-permgen-space <space in MB>
      default is 1/10th of heap space

    -- <any>* 
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
  if tty -s; then
    saved_stty=$(stty -g 2>/dev/null)
  else
    saved_stty=""
  fi
}

scala_exit_status=127
main $*
onExit
