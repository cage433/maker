package starling.schemaevolution

import starling.db.DBWriter
import starling.richdb.RichDB
import system.{Patch, PatchUtils}
import starling.services.StarlingInit

class Patch32_ClearPivotLayoutTable extends Patch {
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    writer.update(PatchUtils.getFileFromClasspath("/schemaevolution/Patch32_ClearPivotLayoutsTable.sql"))
  }

  def patchDescription = "Regenerates the PivotLayout table in a sensible way"
}