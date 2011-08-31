package starling.schemaevolution

import system.Patch
import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.db.DBWriter

class Patch114_BookmarkBundle extends Patch {
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    writer.update("alter TABLE Bookmarks add bundle varchar(128)")
    writer.update("update Bookmarks set bundle = 'StarlingServer'")
  }
}