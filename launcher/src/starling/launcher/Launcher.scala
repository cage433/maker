package starling.launcher

import starling.browser.osgi.BrowserBromptonActivator
import starling.singleclasspathmanager.{JettyBromptonActivator, SingleClasspathManager}
import java.net.{ConnectException, Socket, URL}
import starling.gui.osgi.{MetalsGuiBromptonActivator, GuiLaunchParameters, GuiBromptonActivator}
import starling.manager.BromptonActivator

//Starts the gui without osgi
object Launcher {
  def main(args: Array[String]) {
    if (args.length < 4) {
      throw new IllegalArgumentException("You need to specify 3 arguments: hostname, rmi port, servicePrincipalName and serverType")
    }
    println("Args: " + args.toList)
    val rmiHost = args(0)
    val rmiPort = args(1).toInt
    val servicePrincipalName = args(2)
    val serverType = args(3)

    if (args.length == 4) {
      try {
        val socket = new Socket("localhost", 7777)
        socket.close()
      } catch {
        case e:ConnectException => {
          start(rmiHost, rmiPort, servicePrincipalName, serverType)
        }
      }
      val st = args(3)
      val index = st.indexOf("://")
      val s = st.substring(index + 3)
      val url = new URL("http://localhost:7777/" + s)
      val stream = url.openStream()
      stream.close()
    } else {
      start(rmiHost, rmiPort, servicePrincipalName, serverType)
    }
  }

  def startWithUser(overriddenUser:String) {
    if (rmiPort == -1) {
      throw new Exception("You can only run as user once start has been called")
    }
    start(rmiHost, rmiPort, servicePrincipalName, serverType, Some(overriddenUser))
  }

  // These variables are a big hack so we remember what they are when running the start method when changing users.
  private var rmiHost = ""
  private var rmiPort = -1
  private var servicePrincipalName = ""
  private var serverType = ""

  def start(rmiHost: String, rmiPort: Int, servicePrincipalName: String, serverType:String, overriddenUser:Option[String] = None) {

    this.rmiHost = rmiHost
    this.rmiPort = rmiPort
    this.servicePrincipalName = servicePrincipalName
    this.serverType = serverType

    val launchParameters = GuiLaunchParameters(rmiHost, rmiPort, servicePrincipalName, overriddenUser)

    val baseActivators = List[Class[_ <: BromptonActivator]](classOf[JettyBromptonActivator], classOf[GuiBromptonActivator], classOf[BrowserBromptonActivator])
    val extraActivators = serverType match {
      case "FC2" => List[Class[_ <: BromptonActivator]](classOf[MetalsGuiBromptonActivator])
      case "Oil" => List[Class[_ <: BromptonActivator]]()
    }
    val activators = baseActivators ::: extraActivators
    val single = new SingleClasspathManager(true, activators, List( (classOf[GuiLaunchParameters], launchParameters) ) )
    single.start()
  }
}