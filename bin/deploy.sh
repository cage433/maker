#!/bin/bash

tar xfz starling.tar.gz

cd starling

sbt/sbt "project launcher" update

chmod +x bin/deploy-classpath.sh
