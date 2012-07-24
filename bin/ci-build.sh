#!/bin/bash

echo "Running ci-build.sh"
echo "------------------------------------------------------"
echo "JAVA_HOME: $JAVA_HOME"
echo "SCALA_HOME: $SCALA_HOME"
java -version
$SCALA_HOME/bin/scala -version
echo
echo "pwd: `pwd`"
echo "date: `date`"
echo "uname: `uname -a`"
echo "whoami: `whoami`"
echo "CPUs"
grep processor /proc/cpuinfo
echo "Memory"
free -m
echo "------------------------------------------------------"
echo
echo "##teamcity[progressStart 'compile']"
echo "Script args: $*"
echo "version.number          : $1"
echo "build.type              : $2"
echo "publishing.resolver     : $3"
echo "maker project file name : $4"

ARGS="-Dversion.number=$1 -Dbuild.type=$2 -Dpublishing.resolver=$3"
echo "maker jvm args : $ARGS"

MAKER_PROJECT_FILE=$4

if [ -z "$MAKER_PROJECT_FILE" ];
then
  MAKER_PROJECT_FILE="maker/build-all.scala"
  echo "defaulting build file to $MAKER_PROJECT_FILE"
fi

echo "project file = $MAKER_PROJECT_FILE"

./maker/dist/bin/maker.sh -ntty -c "maker/project/" -p $MAKER_PROJECT_FILE -args $ARGS | tee ci-build.log ; test ${PIPESTATUS[0]} -eq 0 || exit -1

if [ "$2" == "starling" ] && [ "$1" != "" ]; then
  echo "creating and publishing binary deploy artefacts..."
  ./bin/create_artifact.sh $1 $3 || exit 3
fi

