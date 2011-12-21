#!/bin/bash

env JAVACMD=java JAVA_OPTS="-server -Xss128k -Xms6000m -Xmx6000m -XX:-UseConcMarkSweepGC -verbose:gc -XX:+PrintGCTimeStamps -XX:+PrintGCDetails -Dsun.awt.disablegrab=true" scala starling.launcher.DevLauncher

