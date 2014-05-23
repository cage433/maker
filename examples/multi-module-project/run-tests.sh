#!/bin/bash

touch marker-file
../../bin/maker.sh -e 'project.test'

if [ -e "marker-file" ]; then
  exit -1
else
  exit 0
fi
