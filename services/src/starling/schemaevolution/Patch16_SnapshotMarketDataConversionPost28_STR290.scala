package starling.schemaevolution

import com.thoughtworks.xstream.converters.collections.AbstractCollectionConverter
import starling.marketdata.{ForwardRateDataEntry, ImpliedVolData, ImpliedVolEntryKey}
import starling.utils.{TreeMapConverter, Log, OrderedComparable, StarlingXStream}
import com.thoughtworks.xstream.converters.{ConversionException, MarshallingContext, Converter, UnmarshallingContext}
import starling.daterange.{Month, Day}
import starling.market.{FuturesMarket, Market, CommodityMarket}
import starling.marketdata.ImpliedVolEntryKey._
import starling.quantity.{UOM, Percentage}
import xstream.{Fields, Reader, MapBasedConverter}
import com.thoughtworks.xstream.io.{HierarchicalStreamWriter, HierarchicalStreamReader}
import collection.SortedMap
import java.sql.{Statement, ResultSet, Connection}
import starling.props.Props
import starling.db.DBWriter
import starling.richdb.RichDB
import system.Patch
import scala.collection.JavaConversions._
import collection.immutable.{HashMap, TreeMap}
import collection.mutable.ListBuffer
import starling.services.StarlingInit

class Patch16_SnapshotMarketDataConversionPost28_STR290 extends Patch {

  def patchDescription = "Migrate old market data to fit with new monthly markets and converts old Scala2.7 structures"

  val standardXStream = StarlingXStream.createXStream

  val convertingXStream = StarlingXStream.createXStream
  val aliases = Map("scala.$colon$colon" -> classOf[List[_]],
    "scala.collection.jcl.SortedMap$$anon$2" -> classOf[SortedMap[_, _]],
    "map" -> classOf[TreeMap[_, _]],
    "uom" -> classOf[UOM]
    )
  aliases.map{ case (k,v) => convertingXStream.alias(k, v)}

  // Map converter
  convertingXStream.registerConverter(new Converter {
    def canConvert(theType: Class[_]) = {
      classOf[SortedMap[_, _]].isAssignableFrom(theType)
    }

    override def marshal(value: AnyRef, writer: HierarchicalStreamWriter, context: MarshallingContext) = {
      throw new Exception("not marshalling here")
    }

    def unmarshal(reader: HierarchicalStreamReader, context: UnmarshallingContext) = {
      if (reader.getAttributeNames.contains("class") && reader.getAttribute("class") == "map") {
        new TreeMapConverter(convertingXStream.getMapper).unmarshal(reader, context)
      } else {
        val javaMap: java.util.TreeMap[Object, Object] = context.convertAnother(context.currentObject(), classOf[java.util.TreeMap[Object, Object]]).asInstanceOf[java.util.TreeMap[Object, Object]]
        val res: TreeMap[_,_] = if (javaMap.size > 0) {
          javaMap.firstKey match {
            case k: ImpliedVolEntryKey => {
              var bla = new TreeMap[ImpliedVolEntryKey, Percentage]()(ImpliedVolEntryKey)
              val map:java.util.TreeMap[ImpliedVolEntryKey, Percentage] = javaMap.asInstanceOf[java.util.TreeMap[ImpliedVolEntryKey, Percentage]]
              for((k,v) <- map) {
                bla += (k -> v)
              }
              bla
            }
          }
        } else {
          new TreeMap[String, String]()
        }
        res
      }
    }
  })

  convertingXStream.registerConverter(new AbstractCollectionConverter(convertingXStream.getMapper) {
    def canConvert(theType: Class[_]) = {
      classOf[List[_]].isAssignableFrom(theType)
    }

    override def marshal(value: AnyRef, writer: HierarchicalStreamWriter, context: MarshallingContext) = {
      throw new Exception("not marshalling here")
    }

    def unmarshal(reader: HierarchicalStreamReader, context: UnmarshallingContext) = {
      val jm = context.convertAnother(context.currentObject(), classOf[java.util.List[_]]).asInstanceOf[java.util.List[_]]
      val bla = (for(i <- jm) yield i).toList
      bla
    }

    def withChild[T](reader: HierarchicalStreamReader)(op: => T) = {
      reader.moveDown
      val result = op
      reader.moveUp
      result
    }
  } )
  
  var currentMarket: Option[CommodityMarket] = None

  convertingXStream.registerConverter(new MapBasedConverter(
    StarlingXStream.createXStream,
    classOf[ImpliedVolData],
    new Reader(){
      def create(fields: Fields) = {
        if (fields.hasField("surface")) {
          val surface = fields.getFieldValue("surface").asInstanceOf[Option[Map[ImpliedVolEntryKey, Percentage]]].get
          ImpliedVolData(new TreeMap[ImpliedVolEntryKey, Percentage]()(ImpliedVolEntryKey) ++ surface)
        } else if(fields.fields == 0) {
          ImpliedVolData(new TreeMap()(ImpliedVolEntryKey))
        } else {
          throw new Exception("What's this?: " + fields)
        }

      }
    },
    aliases = aliases
  ))

  convertingXStream.registerConverter(new MapBasedConverter(
    StarlingXStream.createXStream,
    classOf[ImpliedVolEntryKey],
    new Reader() {
      def create(fields: Fields) = {
        val exerciseDay = fields.getFieldValue("exerciseDay").asInstanceOf[Option[Day]].get
        val oldPeriodOrLastTrading = fields.hasField("period") match {
          case true => fields.getFieldValue("period").asInstanceOf[Option[Day]].get
          case false => fields.getFieldValue("lastTrading").asInstanceOf[Option[Day]].get
        }
        val strike = fields.getFieldValue("strike").asInstanceOf[Option[Double]].get
        currentMarket match {
          case Some(market) => market match {
            case f: FuturesMarket => {
              val lastTradingDay = f.tenor match {
              // if it's a daily market the lastTradingDay is actually the delivery day
                case Day => f.lastTradingDay(oldPeriodOrLastTrading)
                case Month => oldPeriodOrLastTrading
              }
              val period = if (!f.hasOptions || !f.validLastTradingDay(lastTradingDay) || !f.validOptionExpiry(exerciseDay, lastTradingDay)) {
                oldPeriodOrLastTrading
              } else {
                f.frontPeriod(lastTradingDay)
              }
              ImpliedVolEntryKey(period, strike, exerciseDay)
            }
            case _ => ImpliedVolEntryKey(oldPeriodOrLastTrading, strike, exerciseDay)
          }
          case None => ImpliedVolEntryKey(oldPeriodOrLastTrading, strike, exerciseDay)
        }
      }
    },
    Map[String, Class[_]]("lastTrading" -> classOf[Day]),
    aliases
    ))

  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    val ParseMarket = """([\w\s]+)\s-\s(\d+)""".r

    writer.update("delete from SnapshotData where datatypekey = 'AsianAveragePriceData'")

    val queryString = "Select * from SnapshotData where dataTypeKey in ('AsianVolData', 'ImpliedVolData', 'forwardRates')"
    writer.queryForUpdate(queryString) {
      rs => {
        val snapshotID = rs.getInt("snapshotID")
        val subTypeKey = rs.getString("subTypeKey") match {
          case ParseMarket(name, id) => name
          case k => k
        }

        currentMarket = try {
          Some(Market.fromName(subTypeKey))
        } catch {
          case _ => None
        }

        var xml = rs.getString("data")
        println("Converting " + snapshotID + ", " + subTypeKey)
        try {
          xml = xml.replaceAll("<scala._-colon_-colon>", "")
          xml = xml.replaceAll("serialization=\"custom\"", "")
          xml = xml.replaceAll("</scala._-colon_-colon>", "")
          xml = xml.replaceAll("<unserializable-parents/>", "")
          xml = xml.replaceAll("<scala.ListSerializeEnd_-/>", "")

          val convertedObject = convertingXStream.fromXML(xml)
          currentMarket = None
          val newXML = standardXStream.toXML(convertedObject)
          val postConversion = standardXStream.fromXML(newXML)
          assert(convertedObject == postConversion, convertedObject + " != " + postConversion)
          rs.update(Map("subTypeKey" -> subTypeKey, "data" -> newXML))
        }
        catch {
          case e: ConversionException => {
            println(xml)
            e.printStackTrace
            throw e
          }
        }
      }
    }
  }
}