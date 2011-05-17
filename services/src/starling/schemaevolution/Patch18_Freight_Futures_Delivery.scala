package starling.schemaevolution

import system.Patch
import starling.db.DBWriter
import starling.richdb.RichDB
import starling.utils.Log
import starling.instrument.Future
import starling.market.{Freight, Market, FuturesMarket}
import starling.utils.sql.QueryBuilder._
import starling.services.StarlingInit

class Patch18_Freight_Futures_Delivery extends Patch {
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    val freightMarkets = Market.futuresMarkets.filter(_.commodity == Freight).map(_.name)

    val tables = List("TrinityTrade", "Instrument")
    for (table <- tables) {
      val q = (
              select("*")
                      from (table)
                      where (("instrument" eql Future.name) and ("market" in freightMarkets))
              )

      writer.queryForUpdate(q) {
        rs => {
          // the 'maturity' from trinity is actually the settlement day for freight.
          val settlementDay = rs.getDateRange("delivery").firstDay
          val delivery = settlementDay.containingMonth + -1
          rs.update(Map("delivery" -> delivery))
        }
      }
    }
  }

  def patchDescription = "Fixing the Freight Futures delivery period"
}