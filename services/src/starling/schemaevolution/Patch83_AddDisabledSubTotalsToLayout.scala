package starling.schemaevolution

import system.Patch
import starling.richdb.RichDB
import starling.db.DBWriter
import starling.services.StarlingInit

class Patch83_AddDisabledSubTotalsToLayout extends Patch {
  def patchDescription = "Adds a blank list for disabled sub totals to all layouts"

  protected def runPatch(starlingInit: StarlingInit, starling:RichDB, writer:DBWriter) = {
    val updateText = """<rowSubTotalsDisabled/><columnSubTotalsDisabled reference="../rowSubTotalsDisabled"/>"""

    writer.queryForUpdate("select settings from usersettings") {
      rs => {
        val layout = rs.getString("settings")
        val startIndex = layout.indexOf("</starling.pivot.OtherLayoutInfo>")
        if (startIndex != -1) {
          val (start, end) = layout.splitAt(startIndex)
          val newLayout = start + updateText + end
          rs.update(Map("settings" -> newLayout))
        }
      }
    }

    writer.queryForUpdate("select otherLayoutInfo from PivotLayouts") {
      rs => {
        val layout = rs.getString("otherLayoutInfo")
        val startIndex = layout.indexOf("</starling.pivot.OtherLayoutInfo>")
        if (startIndex != -1) {
          val (start, end) = layout.splitAt(startIndex)
          val newLayout = start + updateText + end
          rs.update(Map("otherLayoutInfo" -> newLayout))
        }
      }
    }
  }
}
