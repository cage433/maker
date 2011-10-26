package starling.schemaevolution

import system.Patch
import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.db.DBWriter
import starling.tradestore.eai.EAITradeStore

class Patch138_AddFFAColumnsToTradeTable extends Patch {
  val tables = "IntradayTrades" :: EAITradeStore.tables

  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) {
    starling.inTransaction {
      writer =>
        tables.map {
          table => {
            writer.update("alter table " + table + " alter column fixedrate varchar(50)")

            writer.update("alter table " + table + " add fixedLegFlatRateYear smallint")
            writer.update("alter table " + table + " add floatingLegFlatRateYear smallint")

            writer.update("alter table " + table + " add fixedLegFlatRate varchar(50)")
            writer.update("alter table " + table + " add fixedLegFlatRateUOM varchar(20)")
          }
        }
    }
  }
}
