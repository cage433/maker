package starling.schemaevolution

import starling.db.DBWriter
import starling.richdb.RichDB
import system.{PatchUtils, Patch}
import starling.services.StarlingInit

class Patch37_RenameRefinedInstruments extends Patch{
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    writer.update(PatchUtils.getFileFromClasspath("/schemaevolution/Patch37_RenameRefinedInstruments.sql"))
  }

  def patchDescription = "Rename the refined instruments to physical/unpriced"
}