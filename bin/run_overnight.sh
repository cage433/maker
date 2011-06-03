#!/bin/bash

cd /home/nick.darcy/workspace/starling

git pull origin master

/home/nick.darcy/apps/java/jdk1.6.0_20/bin/java -jar sbt-jar/sbt-launch.jar "project services" run-overnight
