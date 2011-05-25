package starling.schemaevolution

import starling.richdb.RichDB
import system.{Patch, PatchUtils}
import starling.db.DBWriter
import starling.services.StarlingInit

class Patch14_OptionType extends Patch {
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    writer.update(PatchUtils.getFileFromClasspath("/schemaevolution/Patch14_OptionType.sql"))
  }

  def patchDescription = "Add option type column"
}