#!/bin/bash
java -Xmx4048M -XX:+CMSClassUnloadingEnabled -XX:MaxPermSize=650m -jar -Dsbt.ivy.home=.ivy -Dhttp.nonProxyHosts=nexus.global.trafigura.com sbt/sbt-launch.jar "project databases" "run-main starling.utils.RefreshDatabase $1 $2 $3 $4"
 
