package starling.schemaevolution

import starling.db.DBWriter
import starling.richdb.RichDB
import system.{Patch, PatchUtils}
import starling.services.StarlingInit

class Patch29_AlterSoftmarTradeTable extends Patch {
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    writer.update(PatchUtils.getFileFromClasspath("/schemaevolution/Patch29_AlterSoftmarTradeTable.sql"))
  }

  def patchDescription = "Alter the table that the Softmar trade store use"
}