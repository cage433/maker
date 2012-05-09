#!/bin/bash

echo "Running ci-build.sh"
echo "------------------------------------------------------"
echo "JAVA_HOME: $JAVA_HOME"
echo "SCALA_HOME: $SCALA_HOME"
echo
echo "pwd: `pwd`"
echo "date: `date`"
java -version
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
echo "version.number      : $1"
echo "build.type          : $2"
echo "publishing.resolver : $3"

ARGS="-Dversion.number=$1 -Dbuild.type=$2 -Dpublishing.resolver=$3"
echo "maker jvm args : $ARGS"

./maker/dist/bin/maker.sh -ntty -p maker/build-all.scala -args $ARGS | tee ci-build.log ; test ${PIPESTATUS[0]} -eq 0 || exit -1

