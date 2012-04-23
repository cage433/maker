#!/bin/bash

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
echo "version = $1"

./maker/dist/bin/maker.sh -p maker/build-all.scala
