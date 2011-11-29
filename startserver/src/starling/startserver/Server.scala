package starling.startserver

import starling.services.osgi.ServicesBromptonActivator
import starling.bouncyrmi.BouncyRMIServerBromptonActivator
import starling.auth.osgi.AuthBromptonActivator
import starling.singleclasspathmanager.SingleClasspathManager
import starling.utils.Log
import management.ManagementFactory
import java.io.File
import starling.reports.impl.ReportsBromptonActivator
import starling.rabbiteventviewer.internal.RabbitEventViewerServiceBromptonActivator
import starling.trade.impl.osgi.TradeBromptonActivator
import starling.props.internal.PropsBromptonActivator
import starling.metals.MetalsBromptonActivator
import starling.webservice.HttpWebserviceBromptonActivator
import starling.manager.BromptonActivator
import starling.props.{ServerTypeLabel, Props, PropsHelper}


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
    val props = PropsHelper.defaultProps
    writePIDFile()
    val baseActivators = List[Class[_ <: BromptonActivator]](
      classOf[PropsBromptonActivator],
      classOf[SingleClasspathBroadcasterActivator],
      classOf[AuthBromptonActivator],
      classOf[ServicesBromptonActivator],
      classOf[TradeBromptonActivator],
      classOf[ReportsBromptonActivator],
      classOf[BouncyRMIServerBromptonActivator],
      classOf[HttpWebserviceBromptonActivator]
    )
    val metalsActivators = List[Class[_ <: BromptonActivator]](
      classOf[MetalsBromptonActivator],
      classOf[RabbitEventViewerServiceBromptonActivator]
    )
    val activators = baseActivators ::: (if (props.ServerType() == ServerTypeLabel.FC2) metalsActivators else Nil)
    val single = new SingleClasspathManager(false, activators)
    writePIDFile()
    Log.infoWithTime("Launching starling server") {
      try {
        single.start()
      } catch {
        case e =>
          Log.error("Error launching one of the activators - will exit", e)
          e.printStackTrace
          println("Exiting")
          System.exit(-1)
      }
    }
  }

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
