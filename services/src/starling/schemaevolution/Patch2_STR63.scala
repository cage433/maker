package starling.schemaevolution


import java.sql.Connection
import system.{PatchUtils, Patch}
import starling.db.DBWriter
import starling.richdb.{RichDB}
import starling.services.StarlingInit

class Patch2_STR63 extends Patch {
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    PatchUtils.executeUpdateSQLFileFromClasspath(writer, "/schemaevolution/Patch2_STR63_CreateTablesForReferenceVarReports.sql")
  }

  override protected def checkRunPatchCompletedSuccessfully(starling: RichDB) = {
    //Check the tables exist
    PatchUtils.assertTableExists(starling, "ReferenceVarReportHeader")
    PatchUtils.assertTableExists(starling, "ReferenceVarReportColumn")
    PatchUtils.assertTableExists(starling, "ReferenceVarReportRow")
    PatchUtils.assertTableExists(starling, "ReferenceVarReportValue")
  }
}