package starling.startserver

import starling.services.osgi.ServicesBromptonActivator
import starling.bouncyrmi.BouncyRMIServerBromptonActivator
import starling.auth.osgi.AuthBromptonActivator
import starling.singleclasspathmanager.SingleClasspathManager
import starling.utils.Log
import starling.props.{Props, PropsHelper}


/**
 * Starts the server, without osgi
 */
object Server {

  def main(args:Array[String]) {
    run()
  }

  def run() {
    System.setProperty("log4j.configuration", "utils/resources/log4j.properties")
    PropsHelper.writeDefaults
    val activators = List(
      classOf[SingleClasspathBroadcasterActivator],
      classOf[AuthBromptonActivator],
      classOf[ServicesBromptonActivator],
      classOf[BouncyRMIServerBromptonActivator]
    )
    val single = new SingleClasspathManager(starling.manager.Props.readDefault, activators)
    Log.infoWithTime("Launching starling server") {
      single.start()
    }
  }
}