package starling.schemaevolution

import system.Patch
import starling.db.DBWriter
import starling.richdb.RichDB
import starling.services.StarlingInit

class Patch71_ReimportTrades extends Patch {
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    writer.update("truncate table EAITrade")
    writer.update("truncate table ClosedDesks")
  }

  def patchDescription = "Blank EAI trade and closed desks tables so re-import will happen"
}