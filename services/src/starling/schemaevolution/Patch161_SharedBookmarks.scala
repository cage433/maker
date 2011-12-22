package starling.schemaevolution

import system.Patch
import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.db.DBWriter

class Patch161_SharedBookmarks extends Patch {
  protected def runPatch(starlingInit:StarlingInit, starling:RichDB, writer:DBWriter) {
    val sql = "ALTER TABLE Bookmarks ADD shared bit DEFAULT 0 NOT NULL"
    writer.update(sql)
  }
}