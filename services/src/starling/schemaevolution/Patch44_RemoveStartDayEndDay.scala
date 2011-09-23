package starling.schemaevolution

import system.Patch
import starling.db.DBWriter
import starling.richdb.RichDB
import starling.utils.Log
import starling.market.{Freight, Market, FuturesMarket}
import starling.dbx.QueryBuilder._
import starling.daterange.DateRange
import starling.instrument.{Future}
import starling.services.StarlingInit

class Patch44_UsePeriodFieldForAllDateBasedTradeParameters extends Patch {
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {

    val tables = List("EAITrade", "IntradayTrades", "Instrument")
    for (table <- tables) {
      val rowCount = starling.queryWithOneResult("select count(id) c from " + table, Map()) { rs => rs.getInt("c") }.get
      if (rowCount > 0 || table == "Intradaytrades" || table == "EAITrade") {

        println("Converting table " + table)
        if (table != "EAITrade" && table != "IntradayTrades") { //EAITrade already has DeliveryDay
          writer.update("alter table " + table + " add DeliveryDay datetime")
        }

//        val deliveryDays = starling.queryWithResult( select("distinct Delivery") from (table) where ("Instrument" eql UnpricedAverage.name) and ("Delivery" isNotNull)) { rs =>
//          rs.getDateRange("Delivery")
//        }
//        println("Updating " + deliveryDays.size + " unpriced average delivery days")
//        for (day <- deliveryDays) {
//          writer.update(table, Map("DeliveryDay" -> day), ("Instrument" eql UnpricedAverage.name) and ("Delivery" eql day.toString))
//        }

        writer.update("alter table " + table + " alter column Delivery varchar(255)")
        writer.update("EXEC sp_rename @objname = '" + table + ".Delivery', @newname = 'Period', @objtype = 'COLUMN'")
        val pairs = starling.queryWithResult( select("distinct StartDate, EndDate") from (table) where ("StartDate" isNotNull) and ("EndDate" isNotNull)) { rs =>
          (rs.getDay("StartDate"), rs.getDay("EndDate"))
        }
        println("Updating " + pairs.size + " StartDate EndDate pairs")
        for ( (start,end) <- pairs) {
          val delivery = DateRange(start, end)
          writer.update(table, Map("Period" -> delivery.toString), ("StartDate" eql start) and ("EndDate" eql end))
        }

        val startDays = starling.queryWithResult( select("distinct StartDate") from (table) where ("StartDate" isNotNull) and ("EndDate" isNull)) { rs =>
          rs.getDay("StartDate")
        }
        println("Updating " + startDays.size + " StartDate values")
        for ( start <- startDays) {
          writer.update(table, Map("Period" -> start.toString), ("StartDate" eql start) and ("EndDate" isNull))
        }

        writer.update("alter table " + table + " drop column StartDate")
        writer.update("alter table " + table + " drop column EndDate")

        val spreads = starling.queryWithResult( select("distinct FirstSpreadPeriod, SecondSpreadPeriod") from (table) where ("FirstSpreadPeriod" isNotNull)) { rs =>
          (rs.getDateRange("FirstSpreadPeriod"), rs.getDateRange("SecondSpreadPeriod"))
        }
        println("Updating " + spreads.size + " spread periods")
        for ((first, last) <- spreads) {
          writer.update(table, Map("Period" -> (first + "/" + last)), ("FirstSpreadPeriod" eql first.toString) and ("SecondSpreadPeriod" eql last.toString))
        }
        writer.update("alter table " + table + " drop column FirstSpreadPeriod")
        writer.update("alter table " + table + " drop column SecondSpreadPeriod")
      }
    }

//    //Softmar
//    println("Converting table Softmar")
//    writer.update("alter table SoftmarTrade add Period varchar(255)")
//    val pairs = starling.queryWithResult( select("distinct StartDate, EndDate") from ("SoftmarTrade") where ("StartDate" isNotNull) and ("EndDate" isNotNull)) { rs =>
//      (rs.getDay("StartDate"), rs.getDay("EndDate"))
//    }
//    println("Updating " + pairs.size + " StartDate EndDate pairs")
//    for ( (start,end) <- pairs) {
//      val delivery = DateRange(start, end)
//      writer.update("SoftmarTrade", Map("Period" -> delivery.toString), ("StartDate" eql start) and ("EndDate" eql end))
//    }
//
//    writer.update("alter table SoftmarTrade drop column StartDate")
//    writer.update("alter table SoftmarTrade drop column EndDate")

  }

  def patchDescription = "Use Period field for all date based trade parameters"
}