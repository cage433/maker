#/bin/sh

##
# For automatically finding the revision that broke the regression tests
# To run do:
# git bisect start bad-revision good-revision
# git bisect run bin/regression.sh

BUILD_PATH=$(dirname $0)
cd $BUILD_PATH
cd ..

sbt/sbt compile || exit 125 # skip revision if it doesn't build

sbt/sbt "project services" run-regression

