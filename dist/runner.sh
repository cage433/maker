#!/bin/bash

echo "Running runner.sh"
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

PASS=$1
shift 1
echo "Script args: $*"

# bootstrap and self-build, generate documentation artifacts and publish snapshot to OSS snapshot repo
./bin/maker.sh -y -b -e 'clean;update;test;doc;Publish("maker-oss-snapshot","1.0-SNAPSHOT").execute' -args -Dmaker.ivy.publish.password=$PASS | tee build.log ; test ${PIPESTATUS[0]} -eq 0 || exit -1

echo "done"
