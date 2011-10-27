package starling.schemaevolution

import system.Patch
import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.db.DBWriter
import starling.dbx.QueryBuilder
import starling.utils.ImplicitConversions._
import QueryBuilder._
import xml.{Utility, Elem, Text}

class Patch138_FixMarketDataValueKeys2 extends Patch {

  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) {

    val valueKeyToType =
        starling.queryWithResult("""
        select distinct mdvk.id id, mdek.marketDataType t from MarketDataValueKey mdvk
        join MarketDataValue mdv on mdv.valueKey = mdvk.id
        join MarketDataExtendedKey mdek on mdv.extendedKey = mdek.id
        where mdvk.id in (18, 10184, 1796, 696)
""") { rs => rs.getInt("id") -> rs.getString("t")}.toMultiMap

    val typesToFields = Map(
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
      val filtered = data.child.flatMap(Utility.trimProper).filter {
        case <entry><string>{Text(fieldName)}</string>{_}</entry> => { validFields.contains(fieldName) }
        case o => true
      }
      data.copy(child=filtered)
    }

    var fixCount = 0
    starling inTransaction { writer => {
      starling.query("select * from MarketDataValueKey where id in (18, 10184, 1796, 696)") {
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
              fixCount+=1
              writer.update("MarketDataValueKey", Map("valueKey"->fixed), ("id" eql id))
            }
          } }
        }
      }
    }}
    println("Fixed " + fixCount + " MarketDataValueKey rows")

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
      println("Removed " + dataToIds.values.map(_.size - 1).sum + " duplicated MarketDataValueKey rows")
    }}
  }
}
