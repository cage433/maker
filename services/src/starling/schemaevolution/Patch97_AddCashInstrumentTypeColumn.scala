package starling.schemaevolution

import system.Patch
import starling.db.DBWriter
import starling.richdb.RichDB
import starling.services.StarlingInit

class Patch97_AddCashInstrumentTypeColumn extends Patch {
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    val tables = List("IntradayTrades", "EAITrade", "GalenaTrade", "SoftmarTrade", "TrinityTrade")
    for (table <- tables) {
      writer.update("alter table " + table + " add CashInstrumentType varchar(255) default null")
    }
  }

  def patchDescription = "Add column for CashInstrumentType"
}