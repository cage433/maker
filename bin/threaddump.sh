#!/bin/bash

BUILD_PATH=$(dirname $0)
cd $BUILD_PATH
cd ..

if [ -f pid.txt ]; then
    PID=$(cat pid.txt)
    kill -QUIT $PID
else
    echo "Can't find pid.txt file, maybe the program isn't running"
fi