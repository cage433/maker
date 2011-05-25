package starling.schemaevolution

import starling.db.DBWriter
import starling.richdb.RichDB
import system.{Patch, PatchUtils}
import starling.services.StarlingInit

class Patch49_ClearUserReportsAndLayoutsTables extends Patch {
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    writer.update(PatchUtils.getFileFromClasspath("/schemaevolution/Patch34_ClearUserReportsTable.sql"))
    writer.update(PatchUtils.getFileFromClasspath("/schemaevolution/Patch49_ClearPivotLayoutsTable.sql"))
  }

  def patchDescription = "Patch49: Clears the UserReports and PivotLayouts table"
}