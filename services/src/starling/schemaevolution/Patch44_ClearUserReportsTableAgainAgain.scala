package starling.schemaevolution

import starling.db.DBWriter
import starling.richdb.RichDB
import system.{Patch, PatchUtils}
import starling.services.StarlingInit

class Patch44_ClearUserReportsTableAgainAgain extends Patch {
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    writer.update(PatchUtils.getFileFromClasspath("/schemaevolution/Patch34_ClearUserReportsTable.sql"))
  }

  def patchDescription = "Patch44: Clears the UserReports table as the format has changed and I can't be bothered writing a conversion"
}