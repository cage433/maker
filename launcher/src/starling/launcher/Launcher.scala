package starling.launcher

import starling.singleclasspathmanager.SingleClasspathManager
import starling.gui.osgi.GuiBromptonActivator
import starling.browser.osgi.BrowserBromptonActivator


//Starts the gui without osgi
object Launcher {
  def main(args: Array[String]) {
    if (args.length != 3) {
      throw new IllegalArgumentException("You need to specify 3 arguments: hostname, rmi port and servicePrincipalName")
    }
    println(List() ++ args)
    val rmiHost = args(0)
    val rmiPort = args(1).toInt
    val servicePrincipalName = args(2)

    start(rmiHost, rmiPort, servicePrincipalName)
  }
  def start(rmiHost: String, rmiPort: Int, servicePrincipalName: String, overriddenUser:Option[String] = None) {
    val props = Map("serverHostname" -> rmiHost, "rmiPort" -> rmiPort.toString, "principalName" -> servicePrincipalName)
    val activators = List(classOf[GuiBromptonActivator], classOf[BrowserBromptonActivator])
    val single = new SingleClasspathManager(props, activators)
    single.start()
  }
}