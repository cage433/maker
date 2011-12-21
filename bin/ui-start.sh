java -server -XX:MaxPermSize=512m -Xss128k -Xms6000m -Xmx6000m -XX:-UseConcMarkSweepGC -verbose:gc -XX:+PrintGCTimeStamps -XX:+PrintGCDetails -Dsun.awt.disablegrab=true  starling.launcher.DevLauncher
