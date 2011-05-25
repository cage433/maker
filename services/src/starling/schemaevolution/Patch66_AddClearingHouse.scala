package starling.schemaevolution

import system.Patch
import starling.db.DBWriter
import starling.richdb.RichDB
import starling.services.StarlingInit

class Patch66_AddClearingHouse extends Patch {
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    val tables = List("IntradayTrades", "EAITrade")
    for (table <- tables) {
      writer.update("alter table " + table + " add clearinghouse varchar(50) not null default ''")
    }
  }

  def patchDescription = "Add column for clearing house"
}