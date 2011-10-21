package starling.schemaevolution

import system.{PatchUtils, Patch}
import starling.db.DBWriter
import starling.richdb.RichDB
import starling.services.StarlingInit

class Patch136_AddEmailsSentTable extends Patch {
  protected def runPatch(init: StarlingInit, starling: RichDB, writer: DBWriter) {
    writer.update(PatchUtils.getFileFromClasspath("/schemaevolution/Patch136_AddEmailsSentTable.sql"))
  }
}