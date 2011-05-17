package starling.schemaevolution

import starling.db.DBWriter
import starling.richdb.RichDB
import system.{Patch, PatchUtils}
import starling.services.StarlingInit

class Patch47_ClearAndUpdateUserReportsTable extends Patch {
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    writer.update(PatchUtils.getFileFromClasspath("/schemaevolution/Patch34_ClearUserReportsTable.sql"))
    writer.update(PatchUtils.getFileFromClasspath("/schemaevolution/Patch47_UpdateUserReportsTable.sql"))
  }

  def patchDescription = "Patch47: Clears the UserReports table and adds a column to dictate whether to show parameters"
}