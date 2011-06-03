#!/bin/bash

BUILD_PATH=$(dirname $0)
cd $BUILD_PATH

./stop.sh
sleep 1
cd ..
git pull
cd $BUILD_PATH
sleep 2
./start.sh

