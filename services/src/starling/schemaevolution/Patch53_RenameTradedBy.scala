package starling.schemaevolution

import system.Patch
import starling.db.DBWriter
import starling.richdb.RichDB
import starling.services.StarlingInit

class Patch53_RenameTradedBy extends Patch {
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    val tables = List("IntradayTrades", "EAITrade")
    for (table <- tables) {
      writer.update("exec sp_rename '" + table + ".tradedBy', 'tradedFor', 'COLUMN'")
    }
  }

  def patchDescription = "Rename tradedBy column to tradedFor"
}