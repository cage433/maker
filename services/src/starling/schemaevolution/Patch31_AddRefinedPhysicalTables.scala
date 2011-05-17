package starling.schemaevolution

import system.{PatchUtils, Patch}
import starling.richdb.RichDB
import starling.db.DBWriter
import starling.services.StarlingInit

class Patch31_AddRefinedPhysicalTables extends Patch{
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    writer.update(PatchUtils.getFileFromClasspath("/schemaevolution/Patch31_CreateRefinedPhysicalTables.sql"))
  }

  def patchDescription = "Add the table that the refined physical trade stores will use"

}