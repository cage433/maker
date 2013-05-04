#!/bin/bash


CLASSPATH=`ls utils/lib_managed/*.jar | xargs | sed 's/ /:/g'`
CLASSPATH=$CLASSPATH:`ls libs/*.jar | xargs | sed 's/ /:/g'`
CLASSPATH=$CLASSPATH:`ls $SCALA_HOME/lib/*.jar | xargs | sed 's/ /:/g'`
for module in utils plugin maker; do
  CLASSPATH=$CLASSPATH:$module/target/classes/:$module/target/test-classes/:$module/resources
done
CLASSPATH=$CLASSPATH:resources/
export CLASSPATH
