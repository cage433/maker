package starling.schemaevolution

import system.Patch
import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.db.DBWriter
import starling.dbx.QueryBuilder._
import starling.tradestore.eai.EAITradeStore

class Patch130_AddRoundingMethodRuleToTradeTables extends Patch {
  val tables = "IntradayTrades" :: EAITradeStore.tables.filterNot(_.contains("150"))

  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) {
    starling.inTransaction {
      writer =>
        tables.map {
          table => {
            writer.update("alter table " + table + " add RoundingMethodRule varchar(25) default null")
            writer.update("update " + table + " set RoundingMethodRule = 'per Formula' where instrument = 'Commodity Swap'")
            writer.update("update " + table + " set RoundingMethodRule = 'per Quote' where instrument = 'Commodity Swap' and tradeday >= '2011-9-7'")
          }
        }
    }
  }

}
