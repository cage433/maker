package starling.utils


import java.io.File
import java.io.FileInputStream
import scala.collection.JavaConversions
import java.util.Properties

/*
  Reads properties from Starling's props.conf and Refined's trafigura.properties
*/
object PropertiesMapBuilder{
  val defaultPropsFile = new File("props.conf")

  // To avoid the need for refined clients to have a props.conf file we also read properties
  // from the standard refined config file. To avoid name clashes any Starling properties
  // set there will be prefixed with "Starling". This will then get stripped out before
  // adding to our own properties
  val trafiguraPropertiesFile = new File(System.getenv("HOME") + "/trafigura.properties")

  val defaultProps = {
    val starlingProps = propsFromFile(defaultPropsFile)
    val starlingPrefix = "Starling"
    val trafiguraProps = propsFromFile(trafiguraPropertiesFile)
    val starlingPropsInTrafiguraProperties = trafiguraProps.filterKeys(_.startsWith(starlingPrefix)).map{
      case (key, value) => key.substring(starlingPrefix.length) -> value
    }

    // The location of other services is normally held in trafigura.properties, however
    // has a different key from that used in props.conf
    val refinedServiceProps = trafiguraProps.flatMap{
      case ("trafigura.service.tradeservice", value) => Some(("EdmTradeServiceLocation", value))
      case ("trafigura.service.referencedata", value) => Some(("RefDataServiceLocation", value))
      case _ => None
    }
    refinedServiceProps ++ starlingPropsInTrafiguraProperties ++ starlingProps
  }

  def propsFromFile(propsFile:File) : Map[String, String] = {
    println("Attempting to use props from: " + propsFile)
    val p = new Properties()
    if(propsFile.exists) {
      p.load(new FileInputStream(propsFile))
    }
    Map() ++ JavaConversions.asScalaMap(p.asInstanceOf[java.util.Map[String,String]])
  }  
}
