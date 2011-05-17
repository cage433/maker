package starling.schemaevolution

import starling.db.DBWriter
import starling.richdb.RichDB
import system.{Patch, PatchUtils}
import starling.services.StarlingInit

class Patch34_ClearUserReportsTable extends Patch {
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    writer.update(PatchUtils.getFileFromClasspath("/schemaevolution/Patch34_ClearUserReportsTable.sql"))
  }

  def patchDescription = "Clears the UserReports table as the format has changed and I can't be bothered writing a conversion"
}