package starling.schemaevolution

import starling.db.DBWriter
import starling.richdb.RichDB
import system.{Patch, PatchUtils}
import starling.services.StarlingInit

class Patch33_CreateUserReportsTable extends Patch {
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    writer.update(PatchUtils.getFileFromClasspath("/schemaevolution/Patch33_CreateUserReportsTable.sql"))
  }

  def patchDescription = "Creates the UserReports table where user report configs are saved"
}