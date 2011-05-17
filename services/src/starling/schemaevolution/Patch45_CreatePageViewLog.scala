package starling.schemaevolution

import starling.db.DBWriter
import starling.richdb.RichDB
import system.{PatchUtils, Patch}
import starling.services.StarlingInit

class Patch45_CreatePageViewLog extends Patch {
  protected def runPatch(starlingInit: StarlingInit, starling:RichDB, writer:DBWriter) = {
    writer.update(PatchUtils.getFileFromClasspath("/schemaevolution/Patch45_CreatePageViewLogTable.sql"))
  }

  def patchDescription = "Creates the PageViewLog table where the page views are stored"
}