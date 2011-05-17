#!/bin/sh

java -jar sbt-jar/sbt-launch.jar "project databases" "refresh-database $1 $2 $3 $4"