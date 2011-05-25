package starling.schemaevolution.xstream

import java.lang.reflect.Field
import com.thoughtworks.xstream.XStream
import com.thoughtworks.xstream.io.{HierarchicalStreamReader, HierarchicalStreamWriter}
import com.thoughtworks.xstream.converters.{UnmarshallingContext, Converter, MarshallingContext}
import java.lang.Class
import collection.immutable.HashMap
import starling.utils.StarlingXStream

class UnknownObjectType(field : String) {

  override def toString = {
    "The field " + field + " could not be read " + "because it has no class attribute and the converter does not specify a class"
  }
}

trait Fields {
  def fields: Int

  def hasField(string: String): Boolean

  def getFieldValue(fieldName: String): Option[AnyRef]

  def getText: String
}

trait Reader {
  def create(fields: Fields): AnyRef
}

/**
 * A converter which simplifies writing an XStream converter.
 * You just need to specify the expected types and implement create(Map)
 */
object MapBasedConverter {
  private def getFieldIfExists(k: Class[_], fieldName: String): Field = {
    var field: Field = null
    try {
      field = k.getDeclaredField(fieldName)
    }
    catch {
      case e: SecurityException => {
        throw new RuntimeException(e)
      }
      case e: NoSuchFieldException => {
      }
    }
    if (field == null && k.getSuperclass != null) {
      field = getFieldIfExists(k.getSuperclass, fieldName)
    }
    return field
  }



}

abstract class ModifyClassWhichCanBeReadConverter extends Converter {

  val vanillaXStream = StarlingXStream.createXStream

  def marshal(p1: Any, p2: HierarchicalStreamWriter, p3: MarshallingContext) = {
    throw new IllegalStateException("This should only be used for reading")
  }

  def canConvert(klass : Class[_]) = classToFix == klass

  def fix(any:AnyRef):AnyRef
  def classToFix:Class[_]

  def unmarshal(reader: HierarchicalStreamReader, context: UnmarshallingContext): AnyRef = {
    fix(vanillaXStream.unmarshal(reader))
  }
}

class MapBasedConverter(
  xStream :XStream,
  klass : Class[_],
  myReader : Reader,
  typesOfDroppedFields : Map[String,Class[_]] = Map(),
  aliases: Map[String,Class[_]] = Map()
)
  extends Converter
{
  private def determineClass(className: String): Class[_] = {
    if ("string".equals(className)) {
      return classOf[String]
    }
    if ("hash-map".equals(className)) {
      return classOf[HashMap[_,_]]
    }
    if ("default-map".equals(className)) {
      return classOf[HashMap[_,_]]
    }
    if ("set".equals(className)) {
      return classOf[scala.collection.immutable.TreeSet[_]]
    }
    aliases.get(className) match {
      case Some(klass) => klass
      case None =>
        try {
          return Class.forName(className)
        }
        catch {
          case e => {
            throw new IllegalStateException("ClassName not found. If it looks like a Trafigura class name, such as 'day' for example, " + "then you can add it to MapBasedConverter.", e)
          }
        }
    }
  }


  def marshal(p1: Any, p2: HierarchicalStreamWriter, p3: MarshallingContext) = {
    throw new IllegalStateException("This should only be used for reading")
  }


  def unmarshal(reader: HierarchicalStreamReader, context: UnmarshallingContext): AnyRef = {
    var map: Map[String, AnyRef] = Map.empty[String, AnyRef]
    val text: String = reader.getValue
    while (reader.hasMoreChildren) {
      reader.moveDown
      var nodeName: String = reader.getNodeName
      var fieldClass: Class[_] = null
      var field: Field = getFieldIfExists(nodeName)
      if (field != null) {
        fieldClass = field.getType
      }
      typesOfDroppedFields.get(nodeName) match {
        case Some(specifiedFieldClass) => fieldClass  = specifiedFieldClass
        case None => 
      }

      var classAttribute: String = reader.getAttribute("class")
      if (classAttribute != null) {
        fieldClass = determineClass(classAttribute)
      }
      if (fieldClass == null) {
        map += ((nodeName, new UnknownObjectType(nodeName)))
      }
      else {
        map += ((nodeName, context.convertAnother(null, fieldClass)))
      }
      reader.moveUp
    }
    return myReader.create(
      new Fields {
        def hasField(field: String): Boolean = {
          return map.contains(field)
        }

        def getText: String = {
          return text
        }

        def fields: Int = {
          return map.size
        }

        def getFieldValue(fieldName: String): Option[AnyRef] = {
          var value = map.get(fieldName)
          if (value.isInstanceOf[UnknownObjectType]) {
            throw new IllegalStateException((value.asInstanceOf[UnknownObjectType]).toString)
          }
          return value
        }
      })
  }


  private def getFieldIfExists(fieldName: String): Field = {
    return MapBasedConverter.getFieldIfExists(klass, fieldName)
  }


  def canConvert(p1: Class[_]) = p1 == klass
}

