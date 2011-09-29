package starling.services

import starling.db._
import starling.props.PropsHelper
import starling.utils._
import java.lang.String
import starling.daterange.ObservationTimeOfDay
import collection.SortedMap
import starling.quantity.UOM
import collection.immutable.Map
import starling.marketdata.{MarketDataValueKey, MarketDataTypes, MarketDataKey}
import starling.utils.ImplicitConversions._
import starling.pivot.{Row, Field}

object NewReadAll {
  def main(args: Array[String]) {
    val props = PropsHelper.defaultProps

    val init = new StarlingInit(props)


    Log.infoWithTime("values") {
      init.starlingDB.query("select observationDay, extendedKey, valueKey, value, commitid from MarketDataValues") {
        rs => {
          val day = rs.getDayOption("observationDay")
          val firstKey = rs.getInt("extendedKey")
          val secondKey = rs.getInt("valueKey")
          val value = rs.getDouble("value")
          //        val uom = {
          //          val text = rs.getString("uom")
          //          if (text == "") UOM.NULL else UOM.fromString(text)
          //        }
          val timestamp = rs.getInt("commitid")
        }
      }
    }

    System.exit(0)

    Log.infoWithTime("Readall") {
      val extendedKeys = Log.infoWithTime("extendedKeys") {
        init.starlingDB.queryWithResult("select * from ExtendedMarketDataKey") { MarketDataExtendedKey(_) }.toMapWithKeys(_.id)
      }

      val valueKeys = Log.infoWithTime("valueKeys") {
        init.starlingDB.queryWithResult("select * from ValueKey") { rs => {
          MarketDataValueKey(rs.getInt("id"), Row(rs.getObject[Map[Field, Any]]("value")))
        } }.toMapWithKeys(_.id)
      }

      val commits = Log.infoWithTime("commits") {
        Map() ++ init.starlingDB.queryWithResult("select * from MarketDataCommit") {
          rs => {
            rs.getInt("id") -> rs.getTimestamp("timestamp")
          }
        }
      }

      Log.infoWithTime("values") {
        init.starlingDB.query("select * from MarketDataValues") {
          rs => {
            val day = rs.getDayOption("observationDay")
            val firstKey = extendedKeys(rs.getInt("extendedKey"))
            val secondKey = valueKeys(rs.getInt("valueKey"))
            val value = rs.getDouble("value")
            val uom = {
              val text = rs.getString("uom")
              if (text == "") UOM.NULL else UOM.fromString(text)
            }
            val timestamp = commits(rs.getInt("commitid"))
          }
        }
      }
    }
  }
}