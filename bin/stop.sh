#!/bin/bash

BUILD_PATH=$(dirname $0)
cd $BUILD_PATH
cd ..

if [ -f pid.txt ]; then
    PID=$(cat pid.txt)
    rm pid.txt
    kill $PID
    echo "Process $PID has been sent the kill signal"
    sleep 2
    kill -9 $PID 2> /dev/null
else
    echo
    echo "Can't find pid.txt file, maybe the program isn't running"
    echo
fi
