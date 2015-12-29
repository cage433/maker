#!/bin/bash

# Hack to ensure that maker unit tests that run the tests on this example
# project are working correctly. This script is executed from the maker unit test
# ExampleProjectTests.scala

# Create a dummy file
touch marker-file

# Run this example module's tests - one of them should delete the dummy file
../../maker.py -p Project.scala -E 'singleModule.test' -l logback-config/logback.xml -z

# Return to the maker unit test which invoked this script - ExampleProjectTests.scala
if [ -e "marker-file" ]; then
  exit -1
else
  exit 0
fi
