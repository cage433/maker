#!/bin/bash

if [ $# -lt 1 ]
then
   echo ""
   echo "Must pass in the version number"
   echo ""
   exit 1
fi

VERSION=$1

ARTIFACT=starling-server-$VERSION.gz

wget http://nexus.global.trafigura.com:8081/nexus/content/repositories/starling-releases/starling-releases/starling-server/$VERSION/$ARTIFACT

tar xfz $ARTIFACT

cd starling

IVY_DIR=$HOME/.ivy2

if [ ! -d $IVY_DIR ]; then
   echo "HERE WE ARE"
   tar xfz misc/ivy-preload.tgz
   mv .ivy2 $HOME
fi

sbt/sbt "project launcher" update

