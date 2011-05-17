package starling.schemaevolution

import starling.db.DBWriter
import starling.richdb.RichDB
import system.{PatchUtils, Patch}
import starling.services.StarlingInit


class Patch13_AddEAITradeTable_STR14 extends Patch {
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    writer.update(PatchUtils.getFileFromClasspath("/schemaevolution/Patch13_AddEAITradeTable_STR14.sql"))
  }

  def patchDescription = "Add the table that the EAI trade store will use"
}