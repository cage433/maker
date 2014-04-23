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

eval_file_variable(){
  # indirect references - http://tldp.org/LDP/abs/html/ivr.html
  file=$(eval "echo \$$1")
  if [ -z $file ]; then
    add_error "$(basename ${BASH_SOURCE[0]}) $LINENO: Variable $1 not initialised"
    echo "$1"
  else
    if [ ! -e $file ]; then 
      add_error "$(basename ${BASH_SOURCE[0]}) $LINENO: File $file not found"
    fi
    echo $file
  fi
  [ -e $file ]
}

update_resource(){
  declare local lib line resourceId resource cached_resource resource_cache resolver relativeURL
  read lib line <<<$(echo $*)
  
  resourceId=$(resolve_version $line) 
  resource=$(resource_path $lib $resourceId)
  if [ ! -e $resource ]; then
    resource_cache=$(eval_file_variable GLOBAL_RESOURCE_CACHE) 
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
    add_error "$(basename ${BASH_SOURCE[0]}) $LINENO Failed to update $line - url was $url"
    return 1
  fi
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
  version_file=$(eval_file_variable GLOBAL_RESOURCE_VERSIONS) 
  cat $version_file 2>/dev/null | ( while read key version; do
    line=`echo $line | sed "s/{$key}/$version/g"`
  done
  echo $line )
}

find_resolver(){
  resolver_name=$(lookup_value "resolver" $*)
  resolver_name=${resolver_name:-"default"}

  while read short_name long_name; do
    if [ $resolver_name = $short_name ]; then
      echo $long_name
      return 0
    fi
  done < $(eval_file_variable GLOBAL_RESOURCE_RESOLVERS)
  add_error "$(basename ${BASH_SOURCE[0]}) $LINENO: Unable to find resolver"
  return 1
}
