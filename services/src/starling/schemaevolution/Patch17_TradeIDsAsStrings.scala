package starling.schemaevolution

import starling.db.DBWriter
import starling.richdb.RichDB
import system.{PatchUtils, Patch}
import starling.services.StarlingInit

class Patch17_TradeIDsAsStrings extends Patch {
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    writer.update(PatchUtils.getFileFromClasspath("/schemaevolution/Patch17_TradeIDsAsStrings.sql"))
  }

  def patchDescription = "Convert TradeIDs to Strings"
}