package starling.schemaevolution

import system.Patch
import starling.db.DBWriter
import starling.richdb.RichDB
import starling.services.StarlingInit

class Patch65_TruncateIntradayTrades extends Patch {
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    writer.update("truncate table IntradayTrades")
    //This table has been removed now writer.update("truncate table IntradayTradesUTP")
  }

  def patchDescription = "TruncateIntradayTrades"
}