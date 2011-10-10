package starling.schemaevolution

import system.Patch
import starling.richdb.RichDB
import starling.db.{UpdateableResultSetRow, DBWriter}
import starling.market.{FuturesMarket, FuturesExchangeFactory, Market}
import starling.market.FuturesExchangeFactory._
import starling.instrument.{AsianOption, FuturesOption}
import starling.dbx.QueryBuilder._
import starling.utils.{Log}
import starling.instrument.utils.StarlingXStream
import starling.services.StarlingInit


class Patch31_FixLMEOptionTenors extends Patch {
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {

    // redundant since we removed old Trinity code
    throw new Exception("Patch31_FixLMEOptionTenors should not be run")


//    val xStream = StarlingXStream.createXStream
//
//    // Change trades from asian to tapo
//    for (table <- List("TrinityTrade", "GalenaTrade", "Instrument")) {
//      val lmeIndexes = TrinityIndex.trinityIndexes.filter {
//        i => i.market match {
//          case f: FuturesMarket => f.exchange == LME
//          case _ => false
//        }
//      }
//      for (index <- lmeIndexes) {
//        Log.info("Updating asian options on " + index + " in " + table)
//        writer.queryForUpdate(
//                select("startDate, endDate, instrument") from (table)
//                        where (
//                        ("instrument" eql AsianOption.name) and
//                                ("market" eql index.name)
//                        )
//                ) {
//          rs: UpdateableResultSetRow => {
//            val avg = rs.getDateRange("startDate", "endDate")
//            // i would like to do this assert but some old asians run on dates from, for example
//            // 2009-01-01 to 2009-02-01, which means january 2009.
////            assert(avg.firstMonth == avg.lastMonth)
//            val newAvg = avg.firstMonth
//            rs.update(Map("instrument" -> TAPO.name, "startDate" -> newAvg.firstDay, "endDate" -> newAvg.lastDay))
//          }
//        }
//      }
//    }

  }

  def patchDescription = "Fix LME Option Tenors, they are currently daily but should be monthly, same with market data."
}