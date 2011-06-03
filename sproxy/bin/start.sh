#!/bin/bash

BUILD_PATH=$(dirname $0)
cd $BUILD_PATH
cd ..

if [ -f pid.txt ]; then
    echo
    echo "It looks like the program is already running as there is a pid.txt file. Please either stop the application or remove the pid.txt file."
    echo
else
    echo
    echo "Starting sproxy..."
    echo

    export MAVEN_OPTS=-Xmx512M
    # -o is offline (we don't need maven to check dependencies)
    # -e gives more complete error messages
    nohup mvn exec:java -e -Dexec.mainClass="sproxy.Run" >> log 2>&1 &    

    #  OutputPIDToFile.scala is used to write pid.txt
fi
