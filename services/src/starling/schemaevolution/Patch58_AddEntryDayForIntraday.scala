package starling.schemaevolution

import system.Patch
import starling.db.DBWriter
import starling.richdb.RichDB
import starling.services.StarlingInit

class Patch58_AddEntryDayForIntraday extends Patch {
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    writer.update("truncate table IntradayTrades")
    writer.update("truncate table IntradayTradesUTP")
    writer.update("alter table IntradayTrades add entryDate datetime")
  }

  def patchDescription = "AddEntryDayForIntraday"
}