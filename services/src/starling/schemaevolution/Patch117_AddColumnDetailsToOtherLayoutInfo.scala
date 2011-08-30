package starling.schemaevolution

import system.Patch
import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.db.DBWriter

class Patch117_AddColumnDetailsToOtherLayoutInfo extends Patch {
  protected def runPatch(starlingInit:StarlingInit, starling:RichDB, writer:DBWriter) {
    val textToFind1 = """</otherLayoutInfo>"""
    val textToFind2 = """</starling.pivot.OtherLayoutInfo>"""
    val textToReplaceWith1 = """<columnDetails><expandToFit>false</expandToFit></columnDetails></otherLayoutInfo>"""
    val textToReplaceWith2 = """<columnDetails><expandToFit>false</expandToFit></columnDetails></starling.pivot.OtherLayoutInfo>"""
    val sql1 = "select bookmark from Bookmarks"
    writer.queryForUpdate(sql1) {
      rs => {
        val text = rs.getString("bookmark")
        val (t, r) = if (text.contains(textToFind1)) {
          (textToFind1, textToReplaceWith1)
        } else {
          (textToFind2, textToReplaceWith2)
        }
        val newText = text.replaceAll(t, r)
        rs.update(Map("bookmark" -> newText))
      }
    }
    val sql2 = "select settings from usersettings"
    writer.queryForUpdate(sql2) {
      rs => {
        val text = rs.getString("settings")
        val (t, r) = if (text.contains(textToFind1)) {
          (textToFind1, textToReplaceWith1)
        } else {
          (textToFind2, textToReplaceWith2)
        }
        val newText = text.replaceAll(t, r)
        rs.update(Map("settings" -> newText))
      }
    }
    val sql3 = "select otherLayoutInfo from PivotLayouts"
    writer.queryForUpdate(sql3) {
      rs => {
        val text = rs.getString("otherLayoutInfo")
        val (t, r) = if (text.contains(textToFind1)) {
          (textToFind1, textToReplaceWith1)
        } else {
          (textToFind2, textToReplaceWith2)
        }
        val newText = text.replaceAll(t, r)
        rs.update(Map("otherLayoutInfo" -> newText))
      }
    }
  }
}
