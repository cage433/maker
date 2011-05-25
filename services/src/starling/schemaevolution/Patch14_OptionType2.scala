package starling.schemaevolution

import starling.richdb.RichDB
import system.{Patch, PatchUtils}
import starling.db.DBWriter
import starling.services.StarlingInit

class Patch14_OptionType2 extends Patch {
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    writer.update(PatchUtils.getFileFromClasspath("/schemaevolution/Patch14_OptionType2.sql"))
  }

  def patchDescription = "Add option type column"
}