package starling.schemaevolution

import system.Patch
import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.db.DBWriter
import starling.dbx.QueryBuilder._

class Patch123_ChangeEAIQuoteIDColumnToInt extends Patch {
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) {
    starling.inTransaction{
      writer => writer.update("ALTER TABLE Markets ALTER COLUMN EAIQuoteID INT")
    }
  }

  override def requiresRestart = true
}
