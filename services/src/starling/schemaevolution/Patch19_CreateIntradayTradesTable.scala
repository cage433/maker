package starling.schemaevolution

import starling.richdb.RichDB
import system.{Patch, PatchUtils}
import starling.db.DBWriter
import starling.services.StarlingInit

class Patch19_CreateIntradayTradesTable extends Patch {
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    writer.update(PatchUtils.getFileFromClasspath("/schemaevolution/Patch19_CreateIntradayTradesTable.sql"))
  }

  def patchDescription = "Create table for intra-day trades."
}
