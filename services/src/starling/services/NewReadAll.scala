package starling.services

import starling.db._
import starling.props.PropsHelper
import starling.utils._
import java.lang.String
import starling.daterange.ObservationTimeOfDay
import starling.marketdata.MarketDataTypes
import starling.pivot.Field
import collection.SortedMap
import starling.quantity.UOM
import collection.immutable.Map
import starling.marketdata.MarketDataKey

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
      val firstKeys = Log.infoWithTime("firstkeys") {
        Map() ++ init.starlingDB.queryWithResult("select * from ExtendedMarketDataKey") {
          rs => {
            val id = rs.getInt("id")
            val time = ObservationTimeOfDay.fromName(rs.getString("observationTime"))
            val marketDataSet = MarketDataSet(rs.getString("marketDataSet"))
            val marketDataType = MarketDataTypes.fromName(rs.getString("marketDataSet"))
            val key = rs.getObject[MarketDataKey]("marketDataKey")
            id -> FirstKey(time, marketDataSet, marketDataType, key)
          }
        }
      }
      val secondKeys = Log.infoWithTime("valueKeys") {
        Map() ++ init.starlingDB.queryWithResult("select * from ValueKey") {
          rs => {
            rs.getInt("id") -> SecondKey(rs.getObject[SortedMap[Field, Any]]("value"))
          }
        }
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
            val firstKey = firstKeys(rs.getInt("extendedKey"))
            val secondKey = secondKeys(rs.getInt("valueKey"))
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
