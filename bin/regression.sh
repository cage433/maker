#/bin/sh

##
# For automatically finding the revision that broke the regression tests
# To run do:
# git bisect start bad-revision good-revision
# git bisect run bin/regression.sh

java -XX:MaxPermSize=256m -Xmx6000m -jar sbt/sbt-launch.jar compile || exit 125 # skip revision if it doesn't build

java -XX:MaxPermSize=256m -Xmx6000m -jar sbt/sbt-launch.jar "project services" run-regression

