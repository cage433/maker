#!/bin/bash

if [ $1 ];then
  HOSTANDPORT=$1
else
  HTTP_PORT=`cat props.conf generated.props.conf | grep -i HttpPort | sed -r 's/HttpPort\s*=\s*//i'`
  HTTP_HOST="localhost"
  HOSTANDPORT=$HTTP_HOST:$HTTP_PORT
fi

wget -qO- --no-proxy --header='Accept: application/json' http://${HOSTANDPORT}/jmx/servers/0/domains/Starling/mbeans/name=Users/attributes/UserDetails/
echo



