package starling.schemaevolution

import starling.db.DBWriter
import starling.richdb.RichDB
import system.{PatchUtils, Patch}
import starling.services.StarlingInit

class Patch70_ClearUserTables extends Patch{
  protected def runPatch(starlingInit: StarlingInit, starling:RichDB, writer:DBWriter) = {
    writer.update(PatchUtils.getFileFromClasspath("/schemaevolution/Patch34_ClearUserReportsTable.sql"))
    writer.update(PatchUtils.getFileFromClasspath("/schemaevolution/Patch49_ClearPivotLayoutsTable.sql"))
    PatchUtils.executeUpdateSQL(writer, "delete from dbo.usersettings;")
  }

  def patchDescription = "Clears all the user tables as I've removed totals from the layout"
}