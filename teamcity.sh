#!/bin/bash

#printing everything I can thing of to help diagnose issues when the build doesn't work on teamcity
echo "Running teamcity.sh"
echo "------------------------------------------------------"
echo "pwd `pwd`"
echo "date: `date`"
java -version
echo "uname: `uname -a`"
echo "whoami: `whoami`"
echo "CPUs"
grep processor /proc/cpuinfo
echo "Memory"
free -m
echo "------------------------------------------------------"
echo
echo "##teamcity[progressStart 'compile']"
#java -Xmx4048M -XX:+CMSClassUnloadingEnabled -XX:MaxPermSize=650m -jar -Dsbt.ivy.home=.ivy -Dhttp.nonProxyHosts=nexus.global.trafigura.com sbt/sbt-launch-0.10.jar 'project devLauncher' "test"
java -Xmx4048M -XX:+CMSClassUnloadingEnabled -XX:MaxPermSize=650m -jar -Dsbt.ivy.home=.ivy -Dhttp.nonProxyHosts=nexus.global.trafigura.com sbt/sbt-launch-0.10.jar "test"
