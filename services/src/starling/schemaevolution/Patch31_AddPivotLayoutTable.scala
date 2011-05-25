package starling.schemaevolution

import starling.db.DBWriter
import starling.richdb.RichDB
import system.{Patch, PatchUtils}
import starling.services.StarlingInit

class Patch31_AddPivotLayoutTable extends Patch {
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    writer.update(PatchUtils.getFileFromClasspath("/schemaevolution/Patch31_AddPivotLayoutsTable.sql"))
  }

  def patchDescription = "Add the table that the pivot layouts will use"
}