#!/bin/bash

if [ $# -lt 1 ]
then
   echo ""
   echo "Must pass in the version number"
   echo ""
   exit 1
fi

BUILD_PATH=$(dirname $0)
cd $BUILD_PATH
cd ../..

COMMIT_ID=`git log | head -1 | awk '{print $2}'`

#FILE_NAME=starling_$COMMIT_ID.tar.gz
FILE_NAME=starling.tar.gz

echo $FILE_NAME

tar \
  --exclude 'starling/project/deployment/artifacts/*' \
  --exclude 'starling/props.conf' \
  --exclude 'starling/generated.props.conf' \
  --exclude 'starling/modulejarcache/*' \
  --exclude 'starling/logs/*' \
  -zcvf $FILE_NAME starling

#mv $FILE_NAME starling/project/deployment/artifacts/$FILE_NAME

if [ $1 == "SNAPSHOT" ]; then
  REPO_URL=http://nexus.global.trafigura.com:8081/nexus/content/repositories/starling-test
else
  REPO_URL=http://nexus.global.trafigura.com:8081/nexus/content/repositories/starling-releases
fi

REPO_ID=nexus

mvn -e deploy:deploy-file \
    -Durl=$REPO_URL \
    -DrepositoryId=$REPO_ID \
    -DgroupId=starling-releases \
    -DartifactId=starling-server \
    -Dversion=$1 \
    -Dpackaging=gz \
    -DuniqueVersion=false \
    -DgeneratePom=false \
    -Dfile=$FILE_NAME

mvn -e deploy:deploy-file \
    -Durl=$REPO_URL \
    -DrepositoryId=$REPO_ID \
    -DgroupId=starling-releases \
    -DartifactId=starling-server \
    -Dclassifier=deploy \
    -Dversion=$1 \
    -Dpackaging=sh \
    -DuniqueVersion=false \
    -DgeneratePom=false \
    -Dfile=starling/bin/deploy.sh

rm $FILE_NAME