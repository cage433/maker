package starling.schemaevolution

import system.Patch
import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.db.DBWriter
import starling.tradestore.eai.EAITradeStore

class Patch136_AddRoundingOverrideToTradeTables extends Patch {
  val tables = "IntradayTrades" :: EAITradeStore.tables.filterNot(_.contains("150"))

  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) {
    starling.inTransaction {
      writer =>
        tables.map {
          table => {
            writer.update("alter table " + table + " add RoundingOverride tinyint default null")
          }
        }
    }
  }
}
