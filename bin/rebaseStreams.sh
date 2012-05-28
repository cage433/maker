#!/bin/bash

# promotes named staging branch into master and then rebases
# all sibling streams from the updated master

if [ $# -lt 1 ]
then
   echo "Usage : $0 <branch_name>"
   echo "e.g. : $0 starling-maker"
   exit 0
fi

BRANCH_NAME="$1"
echo "branch name = $BRANCH_NAME"

run_command(){
  command="$1"
  echo "running $command"
  $command || (echo "failed to run $command " && exit -1)
}

# merge the stream forward into [maker-]master
run_command "git checkout $BRANCH_NAME" || exit -1
run_command "git pull" || exit -1

run_command "git checkout maker-master" || exit -1
run_command "git pull" || exit -1

run_command "git merge $BRANCH_NAME" || exit -1
run_command "git push origin maker-master"

# now rebase [maker-]master back into all the streams
run_command "git checkout starling-maker" || exit -1
run_command "git pull" || exit -1
run_command "git merge maker-master" || exit -1
run_command "git push origin starling-maker" || exit -1

run_command "git checkout invoicing-maker" || exit -1
run_command "git pull" || exit -1
run_command "git merge maker-master" || exit -1
run_command "git push origin invoicing-maker" || exit -1

run_command "git checkout costsandincomes-maker" || exit -1
run_command "git pull" || exit -1
run_command "git merge maker-master" || exit -1
run_command "git push origin costsandincomes-maker" || exit -1

# logistics can't merge until they fix their branches
#run_command "git checkout logistics-maker" || exit -1
#run_command "git pull" || exit -1
#run_command "git merge maker-master" || exit -1
#run_command "git push origin logistics-maker" || exit -1

# todo: and v2 & v2 trade services...


# put back on the promoted stream branch for convienience

run_command "git checkout $BRANCH_NAME" || exit -1

echo "done"

