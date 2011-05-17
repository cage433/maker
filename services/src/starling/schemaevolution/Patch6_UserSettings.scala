package starling.schemaevolution

import system.{PatchUtils, Patch}
import java.sql.Connection
import starling.richdb.{RichDB}
import starling.db.DBWriter
import starling.services.StarlingInit

class Patch6_UserSettings extends Patch {

  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    PatchUtils.executeUpdateSQL(writer, "create table dbo.usersettings (starlinguser varchar(30), settings text);")
  }
}