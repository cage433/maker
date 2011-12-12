package starling.schemaevolution

import system.Patch
import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.db.DBWriter


class Patch155_FixDecimalPlaces extends Patch {
  protected def runPatch(ignore: StarlingInit, starling: RichDB, writer: DBWriter) = starling.inTransaction { writer =>
    writer.queryForUpdate("select settings from usersettings where settings is not null and description = 'ExtraFormattingInfo'") { rs => {
      val from = rs.getString("settings")
      val to = from.replace("<string>percentage</string>", "<string>percent</string>")

      if (to != from) rs.update(Map("settings" → to))
    } }
  }
}