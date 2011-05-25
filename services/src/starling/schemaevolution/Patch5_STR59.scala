package starling.schemaevolution


import java.sql.Connection
import system.{PatchUtils, Patch}
import starling.richdb.{RichDB}
import starling.db.DBWriter
import starling.services.StarlingInit

class Patch5_STR59 extends Patch {

  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    PatchUtils.executeUpdateSQLFileFromClasspath(writer, "/schemaevolution/Patch5_STR59_DeleteAllHistoricalErrors.sql")
  }
}