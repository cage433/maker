package starling.schemaevolution

import system.Patch
import starling.db.DBWriter
import starling.richdb.RichDB
import starling.services.StarlingInit

class Patch67_AddClearedSwaps extends Patch {
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    val tables = List("IntradayTrades", "EAITrade", "GalenaTrade", "SoftmarTrade", "TrinityTrade")
    for (table <- tables) {
      writer.update("alter table " + table + " add cleared [tinyint] default 0 not null")
    }
    writer.update("update EAITrade set cleared = 1, clearinghouse = 'ClearPort' where tradeid like 'C%'")
    writer.update("update IntradayTrades set cleared = 1 where instrument = 'Commodity Swap' and clearinghouse = 'ClearPort'")
  }

  def patchDescription = "Add column for cleared swaps"
}