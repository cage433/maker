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
  if [ -z "$JAVA_HOME" ];
  then
    echo "JAVA_HOME not defined"
    exit -1
  fi
  MAKER_OPTIONS=$*
  echo "maker; processing options: $MAKER_OPTIONS"
  process_options $MAKER_OPTIONS
  saveStty

  update_external_jars && check_for_errors
  if [ -z $MAKER_DEVELOPER_MODE ]; then
    bootstrap_maker_if_required && check_for_errors
    recompile_project_if_required && check_for_errors
  fi

  launch_maker_repl
}

update_external_jars(){
  echo "Updating external jars"

  mkdir -p $GLOBAL_RESOURCE_CACHE
  GLOBAL_RESOURCE_CONFIG="$MAKER_ROOT_DIR/external-resource-config" 

  for dir in "test-reporter" "utils"; do 
    update_resources $MAKER_ROOT_DIR/$dir/lib_managed $MAKER_ROOT_DIR/$dir/external-resources
  done
  

  cat > dynamic-zinc-resource-list <<HERE
com.typesafe.sbt compiler-interface {sbt_version} classifier:sources 
com.typesafe.sbt incremental-compiler {sbt_version} 
com.typesafe.sbt sbt-interface {sbt_version}
HERE
  update_resources $MAKER_ROOT_DIR/zinc-libs dynamic-zinc-resource-list 

  cat > dynamic-scala-resource-list <<HERE
org.scala-lang scala-library {scala_version} classifier:sources path:scala-library-{scala_version}-sources.jar 
org.scala-lang scala-library {scala_version} path:scala-library-{scala_version}.jar
org.scala-lang scala-compiler {scala_version} path:scala-compiler-{scala_version}.jar
org.scala-lang scala-compiler {scala_version} classifier:sources path:scala-compiler-sources-{scala_version}.jar
org.scala-lang scala-reflect {scala_version} path:scala-reflect-{scala_version}.jar
org.scala-lang scala-reflect {scala_version} classifier:sources path:scala-reflect-sources-{scala_version}.jar
org.scala-lang jline {scala_version} path:jline-{scala_version}.jar
org.scala-lang jline {scala_version} classifier:sources path:jline-{scala_version}.jar
HERE
  update_resources $MAKER_ROOT_DIR/scala-libs dynamic-scala-resource-list 

  update_resources $PROJECT_ROOT_DIR/scala-libs dynamic-scala-resource-list  
  rm dynamic-scala-resource-list
  rm dynamic-zinc-resource-list
}

build_jar(){
  read jar_name src_files <<<$(echo $*)
  src_files=$( echo "$src_files" | $FIXCP )

  # Not sure why it's necessary to go to the maker root directory. Get 
  # strange compilation errors otherwise
  local saved_dir=$(pwd)
  cd $MAKER_ROOT_DIR

  echo "Building $jar_name"
  rm -f $jar_name

  if test -x /bin/mktemp; then
    TEMP_OUTPUT_DIR=`mktemp -d maker-tmp-XXXXXXXXXX`
  else
    TEMP_OUTPUT_DIR=maker-tmp-$$-$USER-$RANDOM
    mkdir -p $TEMP_OUTPUT_DIR
  fi

  java -classpath $(scala_jars)${PSEP}$(external_jars) \
    -Dscala.usejavacp=true \
    scala.tools.nsc.Main \
    -d $TEMP_OUTPUT_DIR \
    $src_files 2>&1 \
    | tee $MAKER_ROOT_DIR/vim-compile-output ; test ${PIPESTATUS[0]} -eq 0 || exit -1

  run_command "\"$JAVA_HOME/bin/jar\" cf $jar_name -C $TEMP_OUTPUT_DIR . " || exit -1
  rm -rf $TEMP_OUTPUT_DIR
  cd "$saved_dir"
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
    -Dmaker.home="$MAKER_ROOT_DIR" \
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

    java -classpath "$(scala_jars)${PSEP}$(maker_classpath)" -Dscala.usejavacp=true  scala.tools.nsc.Main -d $PROJECT_DEFINITION_CLASS_DIR $PROJECT_DEFINITION_SRC_FILES || exit -1
  fi
}

maker_classpath(){
  cp=$(external_jars)
  if [ $MAKER_DEVELOPER_MODE ];
  then
    for module in utils maker test-reporter; do
      cp="$cp${PSEP}$MAKER_ROOT_DIR/$module/target-maker/classes${PSEP}$MAKER_ROOT_DIR/$module/target-maker/test-classes/"
    done
  else
    cp="$cp${PSEP}$MAKER_JAR${PSEP}$MAKER_SCALATEST_REPORTER_JAR"
  fi
  echo $cp | $FIXCP
}


run_command(){
  eval "$1" || (echo "failed to run $1 " && exit -1)
}
scala_jars() {
  ls "$MAKER_ROOT_DIR"/scala-libs/*.jar \
    | xargs | sed 's/ /'${PSEP}'/g' | $FIXCP
}
external_jars() {
  ls "$MAKER_ROOT_DIR"/utils/lib_managed/*.jar \
     "$MAKER_ROOT_DIR"/test-reporter/lib_managed/*.jar \
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
      -b | --boostrap ) MAKER_BOOTSTRAP=true; shift;;
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

    -b, --boostrap 
      DEPRECATED
      Builds maker.jar from scratch. This should happen automatically if the
      jar is older than any maker source file.

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
