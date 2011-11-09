#!/bin/bash

BUILD_PATH=$(dirname $0)
cd $BUILD_PATH
cd ..

sbt/sbt "project databases" "run-main starling.databases.utils.RefreshDatabase $1 $2 $3 $4"
 
