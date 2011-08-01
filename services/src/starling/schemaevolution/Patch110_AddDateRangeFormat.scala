package starling.schemaevolution

import system.Patch
import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.db.DBWriter
import starling.utils.sql.QueryBuilder._
import starling.gui.StandardUserSettingKeys
import starling.pivot.PivotFormatter


class Patch110_AddDateRangeFormat extends Patch {
  protected def runPatch(starlingInit:StarlingInit, starling:RichDB, writer:DBWriter) {
    val allSettings = starlingInit.userSettingsDatabase.allSettings.toMap
    writer.delete("usersettings", ("starlinguser" in allSettings.keySet))
    val newSettings = allSettings.map{case (u, s) => {
      val extraFormatInfo = s.getSetting(StandardUserSettingKeys.ExtraFormattingInfo, PivotFormatter.DefaultExtraFormatInfo)
      s.putSetting(StandardUserSettingKeys.ExtraFormattingInfo, extraFormatInfo.copy(dateRangeFormat = PivotFormatter.DefaultDateRangeFormat))
      (u, s)
    }}.toList
    starlingInit.userSettingsDatabase.saveAllSettings(newSettings)
  }
}