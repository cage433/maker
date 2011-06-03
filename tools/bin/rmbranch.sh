#!/bin/sh

PROJECT_CODE=str

if [ $# -lt 1 ]
then
   echo "Usage : $0 <JIRA-NUMBER>"
   exit 1
fi

NUM=$1
BRANCH=$PROJECT_CODE-$NUM
ORIGIN=origin
MASTER=master
REMOTE_BRANCH=$ORIGIN/$BRANCH

echo "Deleting branch $BRANCH"
git checkout -q $MASTER
git branch -d $BRANCH
git branch -r -d $REMOTE_BRANCH
git push $ORIGIN :$BRANCH
echo "Deleted branch $BRANCH local and remote"

exit 0
