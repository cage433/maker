#!/bin/bash

BUILD_PATH=$(dirname $0)
cd $BUILD_PATH
cd ../..

COMMIT_ID=`git log | head -1 | awk '{print $2}'`

FILE_NAME=starling_$COMMIT_ID.tar.gz

echo $FILE_NAME

tar --exclude 'starling/project/deployment/artifacts/*' --exclude 'starling/props.conf' -zcvf $FILE_NAME starling

mv $FILE_NAME starling/project/deployment/artifacts/$FILE_NAME