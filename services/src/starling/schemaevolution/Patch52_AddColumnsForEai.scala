package starling.schemaevolution

import system.Patch
import starling.db.DBWriter
import starling.richdb.RichDB
import starling.services.StarlingInit

class Patch52_AddColumnsForEai extends Patch {
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    val tables = List("IntradayTrades", "EAITrade")
    for (table <- tables) {
      writer.update("alter table " + table + " add trader varchar(50) null")
      writer.update("alter table " + table + " add tradedBy varchar(50) null")
      writer.update("alter table " + table + " add broker varchar(50) null")
    }
    writer.update("alter table IntradayTrades add comment varchar(500) null")
  }

  def patchDescription = "Add columns needed for EAI trade blotter"
}