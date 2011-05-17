#!/bin/sh

PROJECT_CODE=str
TRACKED_BRANCH=master

if [ $# -lt 1 ]
then
   echo "Usage : $0 <JIRA-NUMBER>"
   exit 1
fi

NUM=$1
BRANCH=$PROJECT_CODE-$NUM
ORIGIN=origin
REMOTE_BRANCH=$ORIGIN/$BRANCH

echo "Creating branch and tree for $BRANCH"
MATCH="$REMOTE_BRANCH$"
EXISTS=`git branch -r | grep $MATCH | wc -l`
if [ $EXISTS -lt 1 ]
then
   echo "Remote branch $REMOTE_BRANCH not found. So it will be created to track $TRACKED_BRANCH."
   git push $ORIGIN $TRACKED_BRANCH:refs/heads/$BRANCH
   echo "Created remote branch $REMOTE_BRANCH"
fi

echo "Pulling to refresh local repository"
git pull
echo "Pull complete"

MATCH="$BRANCH$"
EXISTS=`git branch | grep $MATCH | wc -l`
if [ $EXISTS -lt 1 ]
then
   echo "Local branch $BRANCH not found. So it will be created."
   git checkout --track -b $BRANCH $REMOTE_BRANCH
   echo "Created local branch $BRANCH"
fi

echo "Done"

exit 0
