#!/bin/bash

function assertEquals { if [ "$1" != "$2" ]; then echo "$3: expected \"$1\", but was: \"$2\""; exit 1; fi }

TARGETS=${@-clean compile}

BUILD_PATH=$(dirname $0)
cd $BUILD_PATH
NAME=$(dirname $(dirname $(pwd)))
cd ..

assertEquals 'Java(TM) SE Runtime Environment (build 1.6.0_25-b06)', "`java -version 2>&1 | grep Env`", 'Invalid Java version'

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

    JMX_PORT=`cat props.conf generated.props.conf | grep -i JmxPort | sed -r 's/JmxPort\s*=\s*//i'`
    if [ ! $JMX_PORT ]; then
      DEFAULT_JMX_PORT=$RANDOM
      echo " Using $DEFAULT_JMX_PORT as the jmx port as there is none set in props.conf or generated.props.conf"
      echo " This is probably because this is the first time starling has been started."
      echo " A unique default is generated on startup."
      JMX_PORT=$DEFAULT_JMX_PORT
    fi

    SERVERNAME=`cat props.conf | grep -i ServerName | sed -r 's/ServerName\s*=\s*//i'`

    echo `java -version`
        echo "Compiling..."
        nohup java \
          -Dhttp.nonProxyHosts=nexus.global.trafigura.com \
          -jar sbt/sbt-launch-0.11.1.jar "project launcher" compile deploy-classpath >> logs/stdouterr.log 2>&1

        COMPILE_WORKED=$?

        if [ $COMPILE_WORKED -eq 0 ]; then
            echo "Starling Starling..."

            . bin/deploy-classpath.sh

            nohup java \
               -DserverName=$SERVERNAME \
               -server \
               -Xmx6000m \
               -XX:MaxPermSize=512m \
               -XX:+HeapDumpOnOutOfMemoryError \
               -Dcom.sun.management.jmxremote.port=$JMX_PORT \
               -Dcom.sun.management.jmxremote.authenticate=false \
               -Dcom.sun.management.jmxremote.ssl=false \
               -Dhttp.nonProxyHosts=nexus.global.trafigura.com \
               starling.startserver.Server >> logs/stdouterr.log 2>&1 &

        else

            echo "There was an error compiling starling - COMPILATION FAILED"

        fi
fi
