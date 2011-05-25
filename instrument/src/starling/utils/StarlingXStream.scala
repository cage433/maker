package starling.utils


import collection.immutable.TreeMap
import collection.SortedMap
import com.thoughtworks.xstream.converters.{Converter, MarshallingContext, UnmarshallingContext}
import starling.daterange.Day
import com.thoughtworks.xstream.io.{HierarchicalStreamWriter, HierarchicalStreamReader}
import com.thoughtworks.xstream.XStream
import starling.quantity.UOM
import scala.collection.JavaConversions
import starling.market.{Index, CommodityMarket, Market, FuturesMarket}
import java.io.StringWriter
import com.thoughtworks.xstream.io.xml.{XppDriver, CompactWriter}
import com.thoughtworks.xstream.mapper.{MapperWrapper, Mapper}
import java.lang.Class
import annotation.tailrec
import starling.pivot.MarketValue

//import starling.db.FwdCurveAppPricingGroup

/**
 * A wrapper around XStream which adds conversions to make the xml more readable
 */
object StarlingXStream {
  private var xstream:Option[XStream] = None
  private var converters: List[Converter] = List()

  def registerConverter(converter: Converter) = xstream match {
    case None => converters ::= converter
    case Some(_) => throw new Exception("XStream has already been configured.")
  }

  object CommodityMarketConverter extends Converter {
    def canConvert(theType:Class[_]) = {
        classOf[CommodityMarket].isAssignableFrom(theType)
    }
    def marshal(obj:Object, writer:HierarchicalStreamWriter, context:MarshallingContext) {
      writer.setValue(obj.asInstanceOf[CommodityMarket].name)
    }
    def unmarshal(reader:HierarchicalStreamReader , context:UnmarshallingContext ) = {
      Market.fromName(reader.getValue)
    }
  }

  object NamedConverter extends Converter {
    def canConvert(theType:Class[_]) = {
      try {
        theType.getMethod("name", Array[Class[_]]() : _*)
        theType.getMethod("fromName", Array[Class[_]](classOf[String]) : _*)
        !classOf[CommodityMarket].isAssignableFrom(theType)
      } catch {
        case e:NoSuchMethodException => false
      }
    }
    def marshal(obj:Object, writer:HierarchicalStreamWriter, context:MarshallingContext) {
      val value = obj.getClass.getMethod("name", Array[Class[_]]() : _*).invoke(obj)
      val text = value match {
        case CaseInsensitive(t) => t
        case s:String => s
        case _ => throw new IllegalStateException(value + " is not a string")
      }
      writer.setValue(text)
    }
    def unmarshal(reader:HierarchicalStreamReader , context:UnmarshallingContext ) = {
       context.getRequiredType.getMethod("fromName", Array[Class[_]](classOf[String]) : _*).invoke(null, Array(reader.getValue) : _*)
    }
  }

  class AnonymousIgnoringMapper(mapper: Mapper) extends MapperWrapper(mapper) {
    override def serializedClass(klazz : Class[_]) = {
      mapper.serializedClass(getNamedClass(klazz))
    }

    def getNamedClass(klazz: Class[_]): Class[_] = {
      if (!isAnon(klazz)) {
        klazz
      }
      else {
        if (!NamedConverter.canConvert(klazz) && !CommodityMarketConverter.canConvert(klazz)) {
          throw new Exception("Cannot persist this anonymous class: " + klazz.getName)
        }
        getNamedClass(klazz.getSuperclass)
      }
    }

    private def isAnon(klazz: Class[_]) = klazz.isAnonymousClass || klazz.getName.contains("$$anon$")
  }

  def createXStream = {
    val anonymousIgnoringMapper = new AnonymousIgnoringMapper(new XStream().getMapper)

    val xs = new XStream(null, anonymousIgnoringMapper, new XppDriver())
    ScalaXStream.configure(xs)

    xs.registerConverter( new Converter {
        def canConvert(theType:Class[_]) = {
            theType == classOf[Day]
        }
        def marshal(obj:Object, writer:HierarchicalStreamWriter, context:MarshallingContext) {
            writer.setValue(obj.toString)
        }
        def unmarshal(reader:HierarchicalStreamReader , context:UnmarshallingContext ) = {
          val text = reader.getValue()
          Day.quickParse(text)
        }
    })

    xs.registerConverter( new Converter {
        def canConvert(theType:Class[_]) = {
            classOf[UOM] == theType
        }
        def marshal(obj:Object, writer:HierarchicalStreamWriter, context:MarshallingContext) {
          writer.setValue(obj.asInstanceOf[UOM].identifier)
        }
        def unmarshal(reader:HierarchicalStreamReader , context:UnmarshallingContext ) = {
          if (reader.getValue == "") UOM.SCALAR else UOM.fromIdentifier(reader.getValue)
        }
    })

    xs.registerConverter(CommodityMarketConverter)
    xs.registerConverter(NamedConverter)

    //On the fly schema changes
    xs.alias("starling.marketdata.MarketValue", classOf[MarketValue])

    converters.foreach(xs.registerConverter)

    xs
  }

  def read(text:String):Any = xstream match {
    case Some(xs) => xs.fromXML(text)
    case None => {
      xstream = Some(createXStream)
      read(text)
    }
  }

  def write(obj:Object): String = {
    try {
      xstream match {
        case Some(xs) => {
          val sw = new StringWriter
          xs.marshal(obj,  new CompactWriter(sw));
          sw.toString
        }
        case None => {
          xstream = Some(createXStream)
          write(obj)
        }
      }
    }
    catch {
      case e => {
        Log.error("XStream write error:" + obj.getClass + ", " + obj, e)
        throw e
      }
    }
  }
}
