package starling.osgirun

object OsgiDevLauncher {

  def main(args:Array[String]) {
    OsgiServer.main(Array())
    DevOsgiGui.main(Array())
  }
}