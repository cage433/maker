#!/bin/bash

BUILD_PATH=$(dirname $0)
cd $BUILD_PATH
cd ..
. bin/deploy-classpath.sh

if [ -f pid.txt ]; then
    echo
    echo "It looks like the program is already running as there is a pid.txt file. Please either stop the application or remove the pid.txt file."
    echo
else
    if [ ! -d logs ]; then
        mkdir logs
    fi
    
    echo "Starting starling..."

    echo $CLASSPATH

    nohup java starling.startserver.Server >> logs/stdouterr.log 2>&1 &
fi