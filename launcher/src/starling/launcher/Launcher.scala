package starling.launcher

import starling.gui.osgi.GuiBromptonActivator
import starling.browser.osgi.BrowserBromptonActivator
import starling.singleclasspathmanager.{JettyBromptonActivator, SingleClasspathManager}
import java.net.{ConnectException, Socket, URL}

//Starts the gui without osgi
object Launcher {
  def main(args: Array[String]) {
    if (args.length < 3) {
      throw new IllegalArgumentException("You need to specify 3 arguments: hostname, rmi port and servicePrincipalName")
    }
    println("Args: " + args.toList)
    val rmiHost = args(0)
    val rmiPort = args(1).toInt
    val servicePrincipalName = args(2)

    try {
      val socket = new Socket("localhost", 7777)
      socket.close()
    } catch {
      case e:ConnectException => {
        start(rmiHost, rmiPort, servicePrincipalName)
      }
    }
    if (args.length == 4) {
      val s = args(3).replaceFirst("starling://", "").replaceFirst("gotoValuationScreen/", "gotoValuationScreen")
      val url = new URL("http://localhost:7777/" + s)
      val stream = url.openStream()
      stream.close()
    }
  }
  def start(rmiHost: String, rmiPort: Int, servicePrincipalName: String, overriddenUser:Option[String] = None) {
    val props = Map("serverHostname" -> rmiHost, "rmiPort" -> rmiPort.toString, "principalName" -> servicePrincipalName)
    val activators = List(classOf[GuiBromptonActivator], classOf[BrowserBromptonActivator], classOf[JettyBromptonActivator])
    val single = new SingleClasspathManager(props, activators)
    single.start()
  }
}