package starling.schemaevolution

import system.Patch
import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.db.DBWriter
import starling.tradestore.eai.EAITradeStore

class Patch137_FixFreightMarketsLimMultiplier extends Patch {
  val tables = "IntradayTrades" :: EAITradeStore.tables.filterNot(_.contains("150"))

  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) {
    starling.inTransaction {
      writer =>
        writer.update("update markets set limMultiplier = 'None' where commodity = 'Freight' and ccy = 'WSC'")
    }
  }
  override def requiresRestart = true
}
