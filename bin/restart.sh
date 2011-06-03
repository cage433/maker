#!/bin/bash

BUILD_PATH=$(dirname $0)
cd $BUILD_PATH

./stop.sh
./start.sh