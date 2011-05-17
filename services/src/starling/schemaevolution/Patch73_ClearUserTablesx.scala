package starling.schemaevolution

import starling.db.DBWriter
import starling.richdb.RichDB
import system.{PatchUtils, Patch}
import starling.services.StarlingInit

class Patch73_ClearUserTablesx extends Patch{
  protected def runPatch(starlingInit: StarlingInit, starling:RichDB, writer:DBWriter) = {
    writer.update(PatchUtils.getFileFromClasspath("/schemaevolution/Patch34_ClearUserReportsTable.sql"))
    writer.update(PatchUtils.getFileFromClasspath("/schemaevolution/Patch49_ClearPivotLayoutsTable.sql"))
    PatchUtils.executeUpdateSQL(writer, "delete from dbo.usersettings;")
  }

  def patchDescription = "Clears all the user tables again as I'm now using sorted maps rather than maps"
}