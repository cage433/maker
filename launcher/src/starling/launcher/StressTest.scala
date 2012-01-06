package starling.launcher

import starling.startserver.Server
import starling.bouncyrmi.BouncyRMIClient
import starling.trade.facility.TradeFacility
import starling.auth.Client
import starling.fc2.api.FC2Facility
import starling.reports.facility.ReportFacility
import starling.props.Props
import starling.api.utils.PropertyValue

object StressTest {
  def main(args:Array[String]) {
    System.setProperty("log4j.configuration", "utils/resources/log4j.properties")
    val props0 = DevLauncher.propsWithUnusedPort()

    val source = "stress"
    val propsMap = props0.starlingProps +
      ("ImportMarketDataAutomatically" -> PropertyValue(source, "false")) +
      ("RabbitEnabled" -> PropertyValue(source, "false")) +
      ("TitanRabbitBrokerHost" -> PropertyValue(source, "")) +
      ("ImportsBookClosesFromEAI" -> PropertyValue(source, "false")) +
      ("QlikViewEnabled" -> PropertyValue(source, "false"))

    val props = new Props(propsMap, props0.trafiguraProps)

    println("")
    println("ImportMarketDataAutomatically " + props.ImportMarketDataAutomatically())
    println("")

    /*System.setProperty("appname", props.ServerName())
    val run = Server.run

    val client = new BouncyRMIClient("localhost", props.RmiPort(), Client.Null)
    val tradeFacility = client.proxy(classOf[TradeFacility])
    val fc2Facility = client.proxy(classOf[FC2Facility])
    val reportFacility = client.proxy(classOf[ReportFacility])


    tradeFacility.init().deskCloses



    run.stop()*/
  }
}