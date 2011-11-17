package starling.schemaevolution

import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.db.DBWriter
import system.{PatchUtils, Patch}


class Patch146_FixInvalidMarketDataValues extends Patch {
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    new MarketDataPatchUtil(starling, writer).removeOrphanedMarketDataValues
    writer.update(PatchUtils.getFileFromClasspath("/schemaevolution/Patch146_FixInvalidMarketDataValues.sql"))
  }

  override def requiresRestart = true
}