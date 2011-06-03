#!/bin/bash

#printing everything I can thing of to help diagnose issues when the build doesn't work on teamcity
echo "Running teamcity.sh"
echo "------------------------------------------------------"
echo "pwd `pwd`"
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
java -Xmx2024M -XX:+CMSClassUnloadingEnabled -XX:MaxPermSize=650m -jar sbt/sbt-launch-0.7.4.jar "test-compile"
if [ "$?" -ne "0" ]; then
  echo "##teamcity[buildStatus status='FAILURE' text='Compile failed']"
else
  echo "##teamcity[progressFinish 'compile']"
  java -Xmx2024M -XX:+CMSClassUnloadingEnabled -XX:MaxPermSize=650m -jar sbt/sbt-launch-0.7.4.jar  "test"
fi
