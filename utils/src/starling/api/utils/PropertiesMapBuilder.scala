package starling.api.utils

import java.util.Properties
import java.io.{FileInputStream, File}
import collection.JavaConversions

object PropertiesMapBuilder{
  val defaultPropsFile = new File("props.conf")

  // To avoid the need for refined clients to have a props.conf file we also read properties
  // from the standard refined config file. To avoid name clashes any Starling properties
  // set there will be prefixed with "Starling". This will then get stripped out before
  // adding to our own properties
  val trafiguraPropertiesFile = new File(System.getenv("HOME") + "/trafigura.properties")

  val starlingProps = propsFromFile(defaultPropsFile)

  val trafiguraProps = {
    val starlingPrefix = "Starling"
    val trafiguraProps = propsFromFile(trafiguraPropertiesFile)
    val starlingPropsInTrafiguraProperties = trafiguraProps.filterKeys(_.startsWith(starlingPrefix)).map{
      case (key, value) => key.substring(starlingPrefix.length) -> value
    }

    val HostURI = """\s*rmi://([^:/ \t]+):(\d+)/\s*""".r

    // The location of other services is normally held in trafigura.properties, however
    // has a different key from that used in props.conf
    val refinedServiceProps = trafiguraProps.flatMap{
      case ("trafigura.service.tradeservice.internal", value) => List(("EdmTradeServiceLocation", value))
      case ("trafigura.service.logistics.internal", value) => List(("LogisticsServiceLocation", value))
      case ("trafigura.service.referencedata.internal", value) => List(("RefDataServiceLocation", value))
      case ("trafigura.service.starling.internal", value) => { 
        try {
          val HostURI(hostName, portNo) = value;  
          List(("ExternalHostname", hostName), ("StarlingServiceRmiPort", portNo)) 
        } catch {
          case _ : MatchError => throw new Exception("malformed starling rmi url - parameter 'trafigura.service.starling.internal' should match '" + HostURI.toString + "'")
        }
      }
      case ("rabbitmq.events.host", value) => List(("TitanRabbitBrokerHost", value))
      case ("rabbitmq.events.username", value) => List(("TitanRabbitUserName", value))
      case ("rabbitmq.events.password", value) => List(("TitanRabbitPassword", value))
      case _ => Nil
    }
    refinedServiceProps ++ starlingPropsInTrafiguraProperties 
  }

  val allProps = trafiguraProps ++ starlingProps
  def property(name : String) : Option[String] = allProps.get(name)

  def propsFromFile(propsFile:File) : Map[String, String] = {
    println("Attempting to use props from: " + propsFile.getAbsolutePath)
    val p = new Properties()
    if(propsFile.exists) {
      p.load(new FileInputStream(propsFile))
    }
    Map() ++ JavaConversions.mapAsScalaMap(p.asInstanceOf[java.util.Map[String,String]])
  }
}
