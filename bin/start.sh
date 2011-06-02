#!/bin/bash

BUILD_PATH=$(dirname $0)
cd $BUILD_PATH
NAME=$(dirname $(dirname $(pwd)))
cd ..
if [ -f pid.txt ]; then
    echo
    echo "It looks like the program is already running as there is a pid.txt file. Please either stop the application or remove the pid.txt file."
    echo
else
    if [ ! -d logs ]; then
        echo
        echo "logs directory doesn't exist - will create it"
        mkdir logs
    fi
    echo "Starting starling..."

    JMX_PORT=`cat props.conf generated.props.conf test_props.conf | grep -i JmxPort | sed -r 's/JmxPort\s*=\s*//i'`
    if [ ! $JMX_PORT ]; then
      DEFAULT_JMX_PORT=$RANDOM
      echo " Using $DEFAULT_JMX_PORT as the jmx port as there is none set in props.conf or generated.props.conf"
      echo " This is probably because this is the first time starling has been started."
      echo " A unique default is generated on startup."
      JMX_PORT=$DEFAULT_JMX_PORT
    fi

    SERVERNAME=`cat props.conf | grep -i ServerName | sed -r 's/ServerName\s*=\s*//i'`

    mkdir -p booter/out/

    # I'm actually going to compile the whole project here because the server needs to serve up gui class files but
    # because it doesn't depend on it, they don't get compiled automatically when you start the server.
    echo "Compiling..."
    nohup java -jar sbt/sbt-launch.jar compile >> logs/fullCompile.log 2>&1 &&

    # Want to compile the booter module (and jar it). It is written in java and used in the exe generation. This should
    # probably be done in SBT.
    echo "Compile booter"
    nohup javac -d booter/out/ booter/src/starling/booter/Booter.java >> logs/booter.log 2>&1 &&

    echo "Starting..."
    nohup java $1 \
       -DserverName=$SERVERNAME \
       -server \
       -Xmx6000m \
      -XX:MaxPermSize=256m \
      -XX:+HeapDumpOnOutOfMemoryError \
      -Dcom.sun.management.jmxremote.port=$JMX_PORT \
      -Dcom.sun.management.jmxremote.authenticate=false \
      -Dcom.sun.management.jmxremote.ssl=false \
      -jar sbt/sbt-launch.jar "project services" run-server >> logs/stdouterr.log 2>&1 &

    #Note: OutputPIDToFile.scala is used to write pid.txt
fi
