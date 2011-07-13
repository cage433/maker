package starling.schemaevolution

import system.Patch
import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.db.DBWriter

class Patch107_CollapsedStateInUserSettings extends Patch {
  protected def runPatch(starlingInit:StarlingInit, starling:RichDB, writer:DBWriter) {
    writer.queryForUpdate("select settings from usersettings") {
      rs => {
        val settings = rs.getString("settings")

        val textToLookFor1 = """<rowCollapsedState>"""
        val textToLookFor2 = """</starling.pivot.OtherLayoutInfo>"""
        val textToUse = """<rowCollapsedState><elements/></rowCollapsedState><columnCollapsedState reference="../rowCollapsedState"/><disabledSubTotals reference="../rowCollapsedState/elements"/></starling.pivot.OtherLayoutInfo>"""

        val startIndex = settings.indexOf(textToLookFor1)
        val endIndex = settings.indexOf(textToLookFor2)

        if (startIndex != -1 && endIndex != -1) {
          val beginText = settings.take(startIndex)
          val finishText = settings.substring(endIndex + textToLookFor2.length())
          val newText = beginText + textToUse + finishText

          rs.update(Map("settings" -> newText))
        }
      }
    }
  }
}