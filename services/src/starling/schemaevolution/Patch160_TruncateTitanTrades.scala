package starling.schemaevolution

import system.Patch
import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.db.DBWriter


class Patch160_TruncateTitanTrades extends Patch{
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    writer.update("truncate table TitanTrade")
    writer.update("delete from ClosedDesks where desk = 'Titan'")
  }

  override def requiresRestart = true
}