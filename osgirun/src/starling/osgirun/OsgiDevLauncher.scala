package starling.osgirun

object OsgiDevLauncher {

  def main(args:Array[String]) {
    OsgiServer.main(Array())
    OsgiGui.main(Array())
  }
}