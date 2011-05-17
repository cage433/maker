package starling.schemaevolution

import system.Patch
import starling.richdb.RichDB
import starling.db.DBWriter
import starling.services.StarlingInit

class Patch80_AddFrozenStateToLayout extends Patch {
  def patchDescription = "Adds frozen state to true to all saved layouts"

  protected def runPatch(starlingInit: StarlingInit, starling:RichDB, writer:DBWriter) = {
    val updateText = """<frozen>true</frozen>"""
    val searchText = "</totals>"
    writer.queryForUpdate("select settings from usersettings") {
      rs => {
        val layout = rs.getString("settings")
        val startIndex = layout.indexOf(searchText)
        val alreadyHasFrozen = layout.indexOf("<frozen>") != -1
        if (!alreadyHasFrozen && startIndex != -1) {
          val (start, end) = layout.splitAt(startIndex + searchText.length)
          val newLayout = start + updateText + end
          rs.update(Map("settings" -> newLayout))
        }
      }
    }

    writer.queryForUpdate("select otherLayoutInfo from PivotLayouts") {
      rs => {
        val layout = rs.getString("otherLayoutInfo")
        val startIndex = layout.indexOf(searchText)
        val alreadyHasFrozen = layout.indexOf("<frozen>") != -1
        if (!alreadyHasFrozen && startIndex != -1) {
          val (start, end) = layout.splitAt(startIndex + searchText.length)
          val newLayout = start + updateText + end
          rs.update(Map("otherLayoutInfo" -> newLayout))
        }
      }
    }
  }
}
