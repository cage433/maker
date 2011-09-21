package starling.startserver

import starling.services.osgi.ServicesBromptonActivator
import starling.bouncyrmi.BouncyRMIServerBromptonActivator
import starling.auth.osgi.AuthBromptonActivator
import starling.singleclasspathmanager.SingleClasspathManager
import starling.utils.Log
import starling.props.{Props, PropsHelper}
import management.ManagementFactory
import java.io.File
import starling.reports.osgi.ReportsBromptonActivator
import starling.rabbiteventviewer.internal.RabbitEventViewerServiceBromptonActivator
import starling.trade.internal.osgi.TradeBromptonActivator
import starling.props.internal.PropsBromptonActivator
import starling.metals.MetalsBromptonActivator


/**
 * The Server object provides a singleton implementation for starting the Starling Server either as a Scala application
 * (using its main(..) method) or programmatically by invoking its run() method.
 * 
 * @see run
 * @documented
 */
object Server {

  /**
   * Invokes this instance's run method.
   * @param args Ignored.
   * @documented
   */
  def main(args:Array[String]) {
    run()
  }

  /**
   * Runs the Starling server.  The log4j configuration is read from the util/resources/log4j.properties. The properties
   * are loaded from the "props.conf" file.  The application's process ID is written to the "pid.txt" file.  If for any
   * reason the server cannot be started (e.g. bad data in the database), a RuntimeException should be thrown.
   */
  def run() {
    System.setProperty("log4j.configuration", "utils/resources/log4j.properties")
    PropsHelper.writeDefaults
    writePIDFile()
    val activators = List(
      classOf[PropsBromptonActivator],
      classOf[SingleClasspathBroadcasterActivator],
      classOf[AuthBromptonActivator],
      classOf[ServicesBromptonActivator],
      classOf[TradeBromptonActivator],
      classOf[ReportsBromptonActivator],
      classOf[BouncyRMIServerBromptonActivator],
      classOf[MetalsBromptonActivator],
      classOf[RabbitEventViewerServiceBromptonActivator]
    )
    val single = new SingleClasspathManager(starling.manager.Props.readDefault, false, activators)
    writePIDFile()
    Log.infoWithTime("Launching starling server") {
      single.start()
    }
  }

  /**
   * Writes the application's process ID (from the ManagementFactory's runtime MX bean's name) to the "pid.txt" file.
   * This file should be deleted on shutdown.
   * @documented
   */
  def writePIDFile() {
    val processName = ManagementFactory.getRuntimeMXBean.getName
    val pid = processName.subSequence(0, processName.indexOf("@")).toString

    val file = new File("pid.txt")
    if (file.exists) file.delete
    val out = new java.io.FileWriter(file)
    out.write(pid + "\n")
    out.close

    Runtime.getRuntime.addShutdownHook(new Thread() {
      override def run() = if (file.exists) file.delete
    })
  }
}
