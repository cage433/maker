#!/bin/bash

echo "Running ci-build.sh"
echo "------------------------------------------------------"
echo "JAVA_HOME: $JAVA_HOME"
echo "SCALA_HOME: $SCALA_HOME"
GIT_COMMIT=`git log -1 | head -n 1 | sed s/commit\ //`
java -version
$SCALA_HOME/bin/scala -version
echo
echo "pwd: `pwd`"
echo "date: `date`"
echo "uname: `uname -a`"
echo "whoami: `whoami`"
echo "CPUs"
grep processor /proc/cpuinfo
echo "Memory"
free -m
echo "git commit: $GIT_COMMIT"
echo "PATH: $PATH"
echo "MAVEN_HOME: $MAVEN_HOME"
echo "MAVEN_OPTS: $MAVEN_OPTS"
echo "------------------------------------------------------"
echo
echo "##teamcity[progressStart 'compile']"
echo "Script args: $*"
echo "build.number            : $1"
echo "build.type              : $2"
echo "publishing.resolver     : $3"
echo "git.commit              : $GIT_COMMIT"
echo "jenkins.job             : $JOB_NAME"
echo "maker project file name : $4"

MAKER_IVY_URL=http://nexus.global.trafigura.com:8081/nexus/content/repositories/central/org/apache/ivy/ivy/2.3.0-rc2/ivy-2.3.0-rc2.jar
MAKER_COMPILER_INTERFACE_URL=http://nexus.global.trafigura.com:8081/nexus/content/repositories/starling-additional-resources/com/typesafe/sbt/compiler-interface-sources/0.12.1/compiler-interface-sources-0.12.1.jar

ARGS="-Dbuild.number=$1 -Dbuild.type=$2 -Dpublishing.resolver=$3 -Dgit.commit=$GIT_COMMIT -Djenkins.job=$JOB_NAME" -Dmaker.test.logback.file=logback.xml
echo "maker jvm args : $ARGS"

MAKER_PROJECT_FILE=$4

if [ -z "$MAKER_PROJECT_FILE" ];
then
  echo "Project file must be specified"
  exit -1
fi

echo "project file = $MAKER_PROJECT_FILE"

export MAKER_DEBUG=true
export no_proxy=$no_proxy,nexus.global.trafigura.com

git clean -f -x -d || exit 4

./maker/dist/bin/maker.sh -d -c "maker/project/" -b -y -d -p $MAKER_PROJECT_FILE --ivy-settings-file maker/ivysettings.xml --ivy-url $MAKER_IVY_URL --compiler-interface-url $MAKER_COMPILER_INTERFACE_URL -args $ARGS | tee ci-build.log ; test ${PIPESTATUS[0]} -eq 0 || exit -1


if [ "$2" == "starling" ] && [ "$3" == "starling-release" ]; then
  echo "creating and publishing binary deploy artefacts..."
  ./bin/create_artifact.sh $1 $3 || exit 3
fi


# To save disk space we do another clean - omitting the -x parameter as
# this would remove scaladoc collected by jenkins
git clean -f -d || exit 4
