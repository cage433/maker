package starling.startserver

import starling.services.osgi.ServicesBromptonActivator
import starling.bouncyrmi.BouncyRMIServerBromptonActivator
import starling.auth.osgi.AuthBromptonActivator
import starling.singleclasspathmanager.SingleClasspathManager
import management.ManagementFactory
import java.io.File
import starling.reports.impl.ReportsBromptonActivator
import starling.rabbiteventviewer.internal.RabbitEventViewerServiceBromptonActivator
import starling.trade.impl.osgi.TradeBromptonActivator
import starling.metals.MetalsBromptonActivator
import starling.webservice.HttpWebserviceBromptonActivator
import starling.manager.BromptonActivator
import starling.props.{ServerTypeLabel, Props, PropsHelper}
import starling.loopyxl.LoopyxlBromptonActivator
import starling.utils.{SingleClasspathBroadcasterActivator, Log}


/**
 * Starts the server, without osgi
 */
object Server {

  def main(args:Array[String]) {
    PropsHelper.writeDefaults
    val props = PropsHelper.defaultProps
    run(props)
  }

  def run(props:Props) = {
    System.setProperty("log4j.configuration", "utils/resources/log4j.properties")
    writePIDFile()
    val baseActivators = List[Class[_ <: BromptonActivator]](
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
      classOf[RabbitEventViewerServiceBromptonActivator],
      classOf[LoopyxlBromptonActivator]
    )
    val activators = baseActivators ::: (if (props.ServerType() == ServerTypeLabel.FC2) metalsActivators else Nil)
    val single = new SingleClasspathManager(true, activators, List( (classOf[Props], props)) )
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
    single
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
