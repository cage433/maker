package starling.schemaevolution

import system.Patch
import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.db.DBWriter
import starling.tradestore.eai.EAITradeStore
import xml.{Elem, Text}
import starling.dbx.QueryBuilder
import starling.utils.ImplicitConversions._
import QueryBuilder._

class Patch137_FixMarketDataValueKeys extends Patch {

  def normaliseValuesWithPrettyPrint(db:RichDB, writer:DBWriter) {
    db.query("select * from MarketDataValueKey") {
      rs => {
        val id = rs.getInt("id")
        val xml = rs.getXML("valueKey")
        writer.update("MarketDataValueKey", Map("valueKey" -> xml), ("id" eql id))
      }
    }
  }

  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) {

    normaliseValuesWithPrettyPrint(starling, writer)

    val valueKeyToType =
        starling.queryWithResult("""
        select distinct mdvk.id id, mdek.marketDataType t from MarketDataValueKey mdvk
        join MarketDataValue mdv on mdv.valueKey = mdvk.id
        join MarketDataExtendedKey mdek on mdv.extendedKey = mdek.id
""") { rs => rs.getInt("id") -> rs.getString("t")}.toMultiMap

    val typesToFields = Map(
      "FreightParityData" -> Set("Contractual Incoterm", "Contractual Location", "Destination Incoterm", "Destination Location"),
      "PriceFixingsHistory" -> Set("Level", "Period"),
      "SpreadStdDevSurface" -> Set("First Period", "Last Period", "Period", "Spread Type", "Delta"),
      "OilVolSurface" -> Set("Period", "Delta"),
      "FreightParity" -> Set[String](),
      "Price" -> Set("Period"),
      "ForwardRate" -> Set("Format", "Day", "Instrument Type"),
      "CountryBenchmark" -> Set("Effective From", "Country Code"),
      "GradeAreaBenchmark" -> Set("Area Code", "Grade Code", "Effective From"),
      "SpotFX" -> Set[String]()
    )

    def filterFields(data:Elem, validFields:Set[String]) = {
      val filtered = data.child.filter {
        case <entry><string>{Text(fieldName)}</string>{_}</entry> => { validFields.contains(fieldName) }
        case _ => true
      }
      data.copy(child=filtered)

    }

    starling inTransaction { writer => {
      starling.query("select * from MarketDataValueKey") {
        rs => {
          val id = rs.getInt("id")
          valueKeyToType.get(id).foreach{ types => {
            val validFieldSets = types.map(typesToFields).toSet
            if (validFieldSets.size > 1) {
              throw new Exception(id + " used by " + types + " which have differing fields " + validFieldSets)
            }
            val validFields = validFieldSets.iterator.next
            val data = rs.getXML("valueKey")
            val fixed = filterFields(data, validFields)
            if (data != fixed) {
              writer.update("MarketDataValueKey", Map("valueKey"->fixed), ("id" eql id))
            }
          } }
        }
      }
    }}

    starling inTransaction { writer => {
      val dataToIds = starling.queryWithResult("select * from MarketDataValueKey") {
        rs => {
          val id = rs.getInt("id")
          val data = rs.getXML("valueKey")
          data -> id
        }
      }.toMultiMap
      dataToIds.foreach { case (data,ids) => {
        if (ids.size > 1) {
          ids.sortWith(_ < _) partialMatch {
            case head :: rest => {
              if (rest.size > 500) {
                println("Too many: " + rest)
              }
              writer.update("MarketDataValue", Map("valueKey" -> head), ("valueKey" in rest))
              writer.delete("MarketDataValueKey", ("id" in rest))
            }
          }
        }
      }}
    }}
  }
}
