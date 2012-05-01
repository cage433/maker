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
echo "version: $1"

./maker/dist/bin/maker.sh -ntty -args "-Dversion.number=$1" -p maker/build-all.scala | tee ci-build.log ; test ${PIPESTATUS[0]} -eq 0 || exit -1

