#!/bin/bash

#Runs a java main supplied as the only argument and records the cpu & gc time

MAIN_CLASS=$1
shift
bin/maker -y -e "update;clean;testCompile;writeStarlingClasspathWithTests"
#bin/maker -e "testCompile;writeStarlingClasspathWithTests"
COMPILE_WORKED=$?

if [ $COMPILE_WORKED -eq 0 ]; then
  . bin/deploy-classpath.sh
  #Note if you use 'time' instead of '/usr/bin/time' the commmand line options don't work. No idea why
  /usr/bin/time --format "%S\n%U\n" --output=cputimes.txt \
   java \
   -server \
   -XX:MaxPermSize=512m \
   -Xms2000m \
   -Xmx4000m \
   -XX:-UseConcMarkSweepGC \
   -verbose:gc \
   -XX:+PrintGCTimeStamps \
   -XX:+PrintGCDetails \
   -Dsun.awt.disablegrab=true \
   -Dlogback.configurationFile=./logback.xml \
   starling.utils.RunMainWithStats $MAIN_CLASS $@

   MAIN_WORKED=$?
   if [ $MAIN_WORKED -eq 1 ]; then
     echo "failed"
   fi

   CPU_MILLIS=`cat cputimes.txt | awk '{s+=$1} END {print s * 1000}'`
   #RunMainWithStats creates the CPU placeholder which is replaced here
   cat times-input.csv | sed s/CPU_MILLIS/$CPU_MILLIS/ > times.csv

   echo "Produced the following times:"
   cat times.csv

   exit $MAIN_WORKED
 else
   echo "compile failed"
   exit 1
 fi


