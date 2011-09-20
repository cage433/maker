#!/bin/bash

env JAVACMD=java JAVA_OPTS="-server -Xss128k -Xmx6000m -Dsun.awt.disablegrab=true" scala starling.launcher.DevLauncher

