package starling.schemaevolution


import java.sql.Connection
import system.{PatchUtils, Patch}
import starling.db.DBWriter
import starling.richdb.{RichDB}
import starling.services.StarlingInit

class Patch3_STR64 extends Patch {

  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    PatchUtils.executeUpdateSQLFileFromClasspath(writer, "/schemaevolution/Patch3_STR64_CreateTableForReportErrors.sql")
  }


  override protected def checkRunPatchCompletedSuccessfully(starling: RichDB) = {
    PatchUtils.assertTableExists(starling, "ReferenceVarReportHeader")
  }
}