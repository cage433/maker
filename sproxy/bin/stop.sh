#!/bin/bash

BUILD_PATH=$(dirname $0)
cd $BUILD_PATH
cd ..

if [ -f pid.txt ]; then
    PID=$(cat pid.txt)
    rm pid.txt
    kill $PID
    echo
    echo "Process $PID has been sent the kill signal"
    echo
else
    echo
    echo "Can't find pid.txt file, maybe the program isn't running"
    echo
fi

