package starling.schemaevolution

import starling.richdb.RichDB
import starling.db.DBWriter
import system.{PatchUtils, Patch}
import starling.services.StarlingInit

class Patch82_CreateUserSystemInfoTable extends Patch {
  def patchDescription = "Creates a table to store users system info"

  protected def runPatch(starlingInit: StarlingInit, starling:RichDB, writer:DBWriter) = {
    writer.update(PatchUtils.getFileFromClasspath("/schemaevolution/Patch82_CreateUserSystemInfoTable.sql"))
  }
}
