#!/bin/bash

rm -rf .maker/maker-libs/
mkdir .maker/maker-libs

echo "Building test-reporter"
source_files=$(find test-reporter/src -name '*.scala' | xargs)
rm -rf tmp-out
mkdir tmp-out
libs=$(ls test-reporter/lib_managed/*.jar | xargs | sed 's/ /:/g')
/usr/local/scala/bin/scalac -cp $libs -d tmp-out $source_files
jar cf .maker/maker-libs/maker-test-reporter.jar -C tmp-out .

echo "Building maker"
rm -rf tmp-out
mkdir tmp-out
utils_source_files=$(find utils/src -name '*.scala' | xargs)
maker_source_files=$(find maker/src -name '*.scala' | xargs)
libs=$(ls utils/lib_managed/*.jar | xargs | sed 's/ /:/g')
/usr/local/scala/bin/scalac -cp $libs -d tmp-out $utils_source_files $maker_source_files
jar cf .maker/maker-libs/maker.jar -C tmp-out .
rm -rf tmp-out
