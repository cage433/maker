package starling.props


import starling.utils.sql.ConnectionParams
import java.io._
import java.lang.reflect.Modifier
import java.util.{Properties => JProperties}
import org.apache.commons.io.IOUtils
import org.apache.commons.io.IOUtils
import scala.collection.JavaConversions
import java.awt.Color
import starling.utils.ImplicitConversions._
import starling.utils.PropertiesMapBuilder

/**
 * Holds the code to manage properties. The properties are listed in Props
 */
class PropsHelper(starlingProps : Map[String,String], trafiguraProps : Map[String, String]) {

  val props = trafiguraProps ++ starlingProps

  object MyPropertiesFile {
    def toLowerCase(propsMap : Map[String, String]) = propsMap.map( (e) => (e._1.toLowerCase() -> e._2) )
    val lowercaseProps = toLowerCase(props)
    def hasProperty(name:String) = lowercaseProps.contains(name.toLowerCase)
    def starlingPropertyNames = toLowerCase(starlingProps).keySet
    def getProperty(name:String, defaultValue:String) : String = lowercaseProps.getOrElse(name.toLowerCase, defaultValue)
  }

  {
    //check for invalid entries
    val invalidNames = MyPropertiesFile.starlingPropertyNames.filter(!properties.contains(_))
    if (!invalidNames.isEmpty) {
      println("There are invalid properties in props.conf. Starling can not start until they are removed.")
      invalidNames.foreach( (name) => println(" " + name))
      System.exit(1)
    }
    //check all properties have a value
    for (property <- properties.valuesIterator) { property.value }
  }

  trait DefaultValue {
    def value():String
  }

  private lazy val properties = {
    Map() ++ this.getClass.getMethods.flatMap {
      method => {
        if (classOf[Property].isAssignableFrom(method.getReturnType) && method.getParameterTypes.isEmpty) {
          Some(method.getName.toLowerCase -> method.invoke(this).asInstanceOf[Property])
        } else {
          None
        }
      }
    }
  }

  def propertiesOfType(propType: Class[_]) = properties.filter(kv => propType.isAssignableFrom(kv._2.getClass))

  class ServerNameStringProperty() extends StringProperty(
      {
        val gitRoot = new File(".").getAbsoluteFile.getParentFile.getParentFile.getParentFile
        if (gitRoot.getName == "starling") {
          val headFile = new File(new File(gitRoot, ".git"), "HEAD")
          val text = IOUtils.toString(new FileInputStream(headFile))
          val slash = text.lastIndexOf("/")
          text.substring(slash+1).trim
        } else {
          gitRoot.getName
        }
      })

  class EmailProperty extends StringProperty(()=> {
      val fullname =
        System.getProperty("user.name") match {
          case "thomas" => "Thomas.Rynne"
          case "dave" => "David.Corcoran"
          case "alex" => "Alex.McGuire"
          case "keith" => "Keith.Davies"
          case "nick" => "Nick.DArcy"
          case "stacy" => "Stacy.Curl"
          case "louis" => "Louis.Botterill"
          case _ => "Thomas.Rynne"
        }
      fullname + "@trafigura.com"
    })

  abstract class Property(defaultGenerator:()=>String) {
    def this(defaultValue:String) = this(()=>defaultValue)
    def value() : String = MyPropertiesFile.getProperty(name, defaultGenerator())
    def name() = {
      val objectName = getClass.getName
      objectName.substring(objectName.indexOf("$")+1, objectName.length()-1)
    }
    def isSet = MyPropertiesFile.hasProperty(name)
    override def toString = {
      name + " = " + value
    }
  }

  abstract class LocalPort(defaultValue: Int) extends IntProperty(defaultValue)

  abstract class IntProperty(defaultValue:Int) extends Property(defaultValue.toString) {
    def apply() = value().toInt
  }
  abstract class BooleanProperty(defaultValue:Boolean) extends Property(defaultValue.toString) {
    def apply() = value().toBoolean
  }
  abstract class StringProperty(default:()=>String) extends Property(default) {
    def this(defaultValue:String) = this(()=>defaultValue)
    def apply() = {
      value()
    }
  }

  abstract class EnumProperty(values0: String*)
    extends Property(values0.headOption.getOrElse("<MISSING>")) {

    private val values = values0.toSet
    def apply() = {
      require(value != "<MISSING>", "EnumProperty '%s' must be constructed with a list of valid values" % name)
      require(values.contains(value), "%s: %s is invalid, valid values are: %s" % (name, value, values.mkString(", ")))

      value
    }
  }

  abstract case class DatabaseProperty(url : String, username : String, password : String)
  	extends Property(url + " " + username + " " + password) 
  {
    def apply() = {
      val split :Array[String] = value().split(' ')
      val url = split(0)
      val (username, password) = split.size match {
        case 2 => (split(1), "")
        case 3 => (split(1), split(2))
        case _ => throw new Exception("Bad split")
      }
      new ConnectionParams(url, username, password)
    }
  }

}

object PropsHelper {

  val defaultProps = new Props(PropertiesMapBuilder.starlingProps, PropertiesMapBuilder.trafiguraProps)

  def createColourString(name:String):String = {
    //Generate a background colour from the name.
    //The format is RRGGBB (red, green, blue)
    //Using dXdXdX means the colour is always reasonably
    //light so it is suitable as a background colour
    val colour = createColour(name)
    "#" +
            Integer.toString(colour.getRed, 16) +
            Integer.toString(colour.getGreen, 16) +
            Integer.toString(colour.getBlue, 16)
  }

  def createColour(name:String):Color = {
    val number = name.hashCode
    val random = new java.util.Random(number)
    val maxColour = 16*16
    val whiteness = 16*8
    new Color(
      random.nextInt(maxColour-whiteness)+whiteness,
      random.nextInt(maxColour-whiteness)+whiteness,
      random.nextInt(maxColour-whiteness)+whiteness
      )    
  }

  /**
   * Writes all the properties which are automatically generated to the file defaults.conf
   * This is generated for use by the proxy
   */
  def writeDefaults() {
    val file = new PrintWriter(new FileOutputStream(new File("generated.props.conf")))
    try {
      file.write( "#NOTE: This file is generated for reference. It is not read by starling\n")
      file.write( "# It is regenerated each time starling starts\n")
      file.write( "\n")
      for (p <- defaultProps.properties.valuesIterator if !p.isSet) {
        file.write( p + "\n")
      }
      file.write( "\n")
      file.write( "#NOTE:  This file is generated for reference. It is not read by starling\n")
    } finally {
      file.close()
    }
  }

  def writeProperties(writer:Writer) {
    writer.write("#remote properties\n")
    writer.write("#Built in properties using default generated values\n")
    for (p <- defaultProps.properties.valuesIterator if !p.isSet) {
      writer.write( p  + "\n")
    }
    writer.write("#Properties specified in props.conf\n")
    for (p <- defaultProps.properties.valuesIterator if p.isSet) {
      writer.write( p  + "\n")
    }
    writer.write("\n")
  }

  def main(args:Array[String]) {
    println("#Built in properties using default generated values")
    for (p <- defaultProps.properties.valuesIterator if !p.isSet) {
      println( p )
    }
    println("#Properties specified in props.conf")
    for (p <- defaultProps.properties.valuesIterator if p.isSet) {
      println( p )
    }
  }
}
