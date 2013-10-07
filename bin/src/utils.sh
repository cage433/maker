#!/bin/bash


has_error(){
  [ -e .maker/maker-shell-errors ]
  return $?
}

add_error(){
  mkdir -p .maker
  echo "  $1" >> .maker/maker-shell-errors
}

has_newer_src_files(){
  read src_dir file_to_compare <<<$(echo $*)
  num_newer=$(find $src_dir -name '*.scala' -newer $file_to_compare | wc -l)
  (( ${num_newer:-"0"} > 0 ))
  return $?
}

resource_basename(){
  declare local default type path classifier groupId artifactId version others 
  read groupId artifactId version others <<<$(echo $*)
  classifier=$(lookup_value "classifier" $others | sed 's/^/-/') # Prepend classifier with a '-'
  type=$(lookup_value "type" $others)
  path=$(lookup_value "path" $others)

  default=$groupId-$artifactId-$version${classifier:=""}.${type:-"jar"}
  echo ${path:-"$default"}
}

resource_path(){
  dir=$1
  shift

  echo $dir/$(resource_basename $*)
}

lookup_value(){
  lookup_key=$1
  shift
  for kv in $*; do
    read key value <<<$(echo $kv | sed 's/:/ /')
    if [ "$key" == "$lookup_key" ]; then
      echo $value
      return
    fi
  done
}

relative_url(){
  read groupId artifactId version others <<<$(echo $*)

  classifier=$(lookup_value "classifier" $others | sed 's/^/-/') # Prepend classifier with a '-'
  type=$(lookup_value "type" $others)

  echo `echo $groupId | sed 's/\./\//g'`/$artifactId/$version/$artifactId-$version${classifier:=""}.${type:="jar"}
}

update_resource(){
  declare local lib line resourceId resource cached_resource resource_cache resolver relativeURL
  read lib line <<<$(echo $*)
  
  resourceId=$(resolve_version $line) 
  resource=$(resource_path $lib $resourceId)
  if [ ! -e $resource ]; then
    resource_cache=${GLOBAL_RESOURCE_CACHE-:"$HOME/.maker-resource-cache"} 
    cached_resource=$(resource_path $resource_cache $resourceId)
    # copy from cache if it exists
    if [ -e $cached_resource ]; then
      cp $cached_resource $resource
    else
      # try to download from one of the resolvers
      resolver=$(find_resolver $resourceId)
      relativeURL=$(relative_url "$resourceId")
      url="$resolver"/"$relativeURL"
      curl $url -s -H "Pragma: no-cache" -f -o $resource
      if [ -e $resource ]; then
        cp $resource $cached_resource
      fi
    fi
  fi

  if [ ! -e $resource ]; then
    add_error "$(basename ${BASH_SOURCE[0]}) $LINENO Failed to update $line (resource attempted: $relativeURL)"
    return 1
  fi
}

download_scala(){
  read resolver resource_cache <<<$(echo $*)
  add_error "Looking for scala version "
  scala_version=$(resolve_version "{scala_version}") 
  add_error "Scala version "$scala_version
  resourceId="scala-"$scala_version".tgz"
  cached_resource=$resource_cache"/"$resourceId

  #url="http://www.scala-lang.org/files/archive/"$resourceId
  url=$resolver/$resourceId
  curl $url -s -H "Pragma: no-cache" -f -o $cached_resource
  if [ ! -e $cached_resource ]; then
    add_error "$(basename ${BASH_SOURCE[0]}) $LINENO Failed to download $url to $cached_resource"
    add_error "CMD was 'curl $url -s -H \"Pragma: no-cache\" -f -o $cached_resource'"
    return 1
  fi
  return 0
}

update_resources(){
  read lib resourceIdFile <<<$(echo $*)
  mkdir -p $lib

  while read line && ! has_error; do
    update_resource $lib $cache $line
  done < $resourceIdFile
  has_error
  return $?
}

resolve_version(){
  line=$*
  while read key version; do
    line=`echo $line | sed "s/{$key}/$version/g"`
  done < <(sed -n '/^[^# \t]/p' ${GLOBAL_RESOURCE_VERSIONS-:"NO_RESOURCE_FILE"} 2>/dev/null) # remove comments and lines beginning with whitespace 
  echo $line 
}

find_resolver(){
  resolver_name=$(lookup_value "resolver" $*)
  resolver_name=${resolver_name:-"default"}

  while read short_name long_name; do
    if [ $resolver_name = $short_name ]; then
      echo $long_name
      return 0
    fi
  done < <(sed -n '/^[^# \t]/p' ${GLOBAL_RESOURCE_RESOLVERS-:"NO_RESOLVER_FILE"} 2>/dev/null) # remove comments and lines beginning with whitespace 

  add_error "$(basename ${BASH_SOURCE[0]}) $LINENO: Unable to find resolver"
  return 1
}
