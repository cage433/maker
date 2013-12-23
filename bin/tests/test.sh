#!/bin/bash

ABS_SCRIPT_PATH=`pwd`/$0
. bin/src/utils.sh

setUp() {
  clearSandbox
  mkdir sandbox
  cd sandbox
  ln -s $ABS_SCRIPT_PATH .
  rm -f /home/alex/foo.log

}

clearSandbox(){
  if [[ `pwd` == *sandbox ]]; then
    cd ..
  fi
  if [ -e sandbox ]; then
    rm -rf sandbox
  fi
}
tearDown() {
  clearSandbox
}

test_lookup_value(){
  line=" foo:bar fred:mike  "
  assertEquals "lookup(foo)=bar" "$(lookup_value "foo" $line)" "bar"
  assertEquals "lookup(fred)=mike" "$(lookup_value "fred" $line)" "mike"
  assertEquals "lookup(fred) in empty line empty" "$(lookup_value "fred" "")" ""
  assertEquals "lookup(bill) in non matching line empty" "$(lookup_value "bill" $line)" ""
  assertEquals "lookup("") empty" "$(lookup_value "" $line)" ""
}

test_relative_url(){
  url=$(relative_url "org.apache.commons" "commons-lang3" "3.1")
  assertEquals "org/apache/commons/commons-lang3/3.1/commons-lang3-3.1.jar" "$url"

  url=$(relative_url "org.apache.commons" "commons-lang3" "3.1" "type:jar")
  assertEquals "org/apache/commons/commons-lang3/3.1/commons-lang3-3.1.jar" "$url"
}

test_resource_basename(){
  path=cache/dir/$(resource_basename "org.apache.commons" "commons-lang3" "3.1")
  assertEquals "cache/dir/org.apache.commons-commons-lang3-3.1.jar" "$path"
}

test_update_copies_cached_resource()
{
  GLOBAL_RESOURCE_CACHE="CACHE"
  mkdir $GLOBAL_RESOURCE_CACHE
  mkdir LIB
  resourceId="org.apache.commons commons-lang3 3.1"
  echo $resourceId > resources
  resourceBasename=$(resource_basename $resourceId)

  echo "" > $GLOBAL_RESOURCE_CACHE/$resourceBasename
  resolver="dummy-resolver"

  update_resources "LIB" resources
  assertTrue "Resource should have been copied from cache" "[ -e LIB/$resourceBasename ]"
}

test_update_downloads_non_cached_resource()
{
  GLOBAL_RESOURCE_CACHE="CACHE"
  mkdir $GLOBAL_RESOURCE_CACHE

  GLOBAL_RESOURCE_CONFIG=resource_config
  echo "resolver: default file://`pwd`/RESOLVER" > $GLOBAL_RESOURCE_CONFIG

  mkdir LIB
  resourceId="org.apache.commons commons-lang3 3.1"
  echo $resourceId > resources
  resourceBasename=$(resource_basename $resourceId)
  resolverPath=RESOLVER/$(relative_url $resourceId)
  mkdir -p $(dirname $resolverPath)
  echo "" > $resolverPath
  update_resources "LIB" resources
  assertTrue "Resource should have been downloaded by curl" "[ -e LIB/$resourceBasename ]"
  assertTrue "Downloaded resource should have been cached" "[ -e CACHE/$resourceBasename ]"
}

test_can_resolve_versions(){
  GLOBAL_RESOURCE_CONFIG=resource_config
  cat > $GLOBAL_RESOURCE_CONFIG <<HERE
version: scala_version 2.10.2
version: jetty_version 7.6.3.v20120416
HERE

  assertEquals "foo2.10.2" $(resolve_version "foo{scala_version}")
  assertEquals "foo2.10.2-7.6.3.v20120416-x" $(resolve_version "foo{scala_version}-{jetty_version}-x")
}

test_can_parse_resolver_list()
{

  GLOBAL_RESOURCE_CONFIG=resource_config
  cat > $GLOBAL_RESOURCE_CONFIG <<HERE
resolver: default file://RESOLVER
# some random comment

# blank line above will be ignored
resolver: other file://OTHER-RESOLVER
HERE

  assertEquals "file://RESOLVER" "$(find_resolver "")"
  assertEquals "file://RESOLVER" "$(find_resolver "resolver:default")"
  assertEquals "file://OTHER-RESOLVER" "$(find_resolver "resolver:other")"
}

test_update_resources()
{
  GLOBAL_RESOURCE_CONFIG=resource_config
  echo "resolver: default file://`pwd`/RESOLVER" >> $GLOBAL_RESOURCE_CONFIG

  cat >> $GLOBAL_RESOURCE_CONFIG <<HERE
version: scala_version 2.10.2
version: jetty_version 7.6.3.v20120416
HERE

   cat > resources <<HERE
org.apache.commons commons-lang3 3.1 resolver:default classifier:sources
org.apache.commons commons-lang3 3.1 resolver:default
org.scalacheck scalacheck_{scala_version} 1.9 resolver:default
HERE


  mkdir CACHE
  mkdir LIB

  mkdir -p RESOLVER/org/scalacheck/scalacheck_2.10.2/1.9
  echo > RESOLVER/org/scalacheck/scalacheck_2.10.2/1.9/scalacheck_2.10.2-1.9.jar
  mkdir -p RESOLVER/org/apache/commons/commons-lang3/3.1
  echo > RESOLVER/org/apache/commons/commons-lang3/3.1/commons-lang3-3.1.jar
  echo > RESOLVER/org/apache/commons/commons-lang3/3.1/commons-lang3-3.1-sources.jar

  update_resources LIB resources 

  assertTrue "commons source jar should exist" "[ -e LIB/org.apache.commons-commons-lang3-3.1-sources.jar ]"
  assertTrue "commons jar should exist" "[ -e LIB/org.apache.commons-commons-lang3-3.1.jar ]"
  assertTrue "scalacheck jar should exist" "[ -e LIB/org.scalacheck-scalacheck_2.10.2-1.9.jar ]"

}

test_jar_not_created_when_update_fails(){
  GLOBAL_RESOURCE_CONFIG=resource_config
  echo "resolver: default file://`pwd`/MISSING-RESOLVER" >> $GLOBAL_RESOURCE_CONFIG
  mkdir CACHE
  mkdir LIB
  resourceId="org.apache.commons commons-lang3 3.1"
  echo $resourceId > resources
  update_resources "LIB" resources
  resourceBasename=$(resource_basename $resourceId)
  assertTrue "Resource should not have been downloaded by curl" "[ ! -e LIB/$resourceBasename ]"
  assertTrue "resource should not have been cached" "[ ! -e CACHE/$resourceBasename ]"
}

test_has_newer_src_files_empty()
{
  mkdir DIR
  result=$(has_newer_src_files DIR DIR)
  assertFalse "Should have no newer src files" $(has_newer_src_files DIR DIR)
}

test_has_newer_src_files_no_directory()
{
  mkdir DIR
  result=$(has_newer_src_files not_a_real_dir DIR)
  assertFalse "Should have no newer src files" $(has_newer_src_files not_a_real_dir DIR)
}
test_has_src_files_one_newer()
{
  touch maker.jar
  mkdir src
  sleep 0.01
  touch src/foo.scala
  has_newer_src_files src maker.jar
  if ! has_newer_src_files src maker.jar; then
    fail "Should have one newer src file"
  fi
}

test_can_download_scala()
{
  GLOBAL_RESOURCE_CONFIG=resource_config
  cat > $GLOBAL_RESOURCE_CONFIG <<HERE
version: scala_version 2.10.3
HERE
  mkdir dummy_resolver
  mkdir -p dummy_scala/lib
  mkdir -p dummy_scala/src
  touch dummy_scala/lib/one.jar
  touch dummy_scala/lib/two.jar
  touch dummy_scala/src/three.jar
  tar czf dummy_resolver/scala-2.10.3.tgz dummy_scala
  GLOBAL_RESOURCE_CACHE="CACHE"
  mkdir $GLOBAL_RESOURCE_CACHE
  download_scala_jars "file://`pwd`/dummy_resolver/scala-2.10.3.tgz" lib
  assertTrue "scala jars should have downloaded" "[ -e lib/one.jar ]"
  assertTrue "scala jars should have downloaded" "[ -e lib/two.jar ]"
  assertTrue "scala jars should have downloaded" "[ -e lib/three.jar ]"
  assertTrue "scala zip should have been cached" "[ -e CACHE/scala-2.10.3.tgz ]"
}

test_lines_beginning_with()
{
  cat > "temp_file" <<HERE
key1 fred
key2 mike
key1 bill

# comment

key1 dave
HERE
  res=$(lines_beginning_with "key1" "temp_file" | xargs)
  assertTrue "Expected 'fred bill dave' got $res." "[ \"fred bill dave\" == \"$res\" ]"
}

. /usr/bin/shunit2

