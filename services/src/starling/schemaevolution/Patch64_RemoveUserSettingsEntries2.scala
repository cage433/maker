package starling.schemaevolution

import system.{PatchUtils, Patch}
import starling.richdb.RichDB
import starling.db.DBWriter
import starling.services.StarlingInit

class Patch64_RemoveUserSettingsEntries2 extends Patch {
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    PatchUtils.executeUpdateSQL(writer, "delete from dbo.usersettings;")
  }
}