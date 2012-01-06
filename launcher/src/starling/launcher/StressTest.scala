package starling.launcher

import starling.startserver.Server

object StressTest {
  def main(args:Array[String]) {
    System.setProperty("log4j.configuration", "utils/resources/log4j.properties")
    val props = DevLauncher.propsWithUnusedPort()
    System.setProperty("appname", props.ServerName())
    val run = Server.run
    Thread.sleep(1000)
    run.stop()
  }
}