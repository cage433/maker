#!/bin/bash

bindir=`dirname "$0"`
$bindir/listusers.sh $1 | ruby -rjson -e 'puts JSON::pretty_generate(JSON::parse(STDIN.read))'

#this requires ruby and libjson-ruby