package starling.schemaevolution

import system.Patch
import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.db.DBWriter

class Patch151_MigrateBookmarkEnvRuleMostRecentToAllCloses extends Patch {
  protected def runPatch(starlingInit:StarlingInit, starling:RichDB, writer:DBWriter) {

    // convert all "Most recent closes" to "All closes" env rule in all bookmarks, as Most recent closes in no longer supported
    val textToFind = """<environmentRule><name>Most recent closes</name></environmentRule>"""
    val textToReplaceWith = """<environmentRule><name>All Closes</name></environmentRule>"""

    val sql = "select bookmark from Bookmarks"

    writer.queryForUpdate(sql) {
      rs => {
        val text = rs.getString("bookmark")
        if (text.contains(textToFind)) {
          val newText = text.replaceAll(textToFind, textToReplaceWith)
          rs.update(Map("bookmark" -> newText))
        }
      }
    }
  }
}
