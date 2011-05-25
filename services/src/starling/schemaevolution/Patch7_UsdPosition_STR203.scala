package starling.schemaevolution

import java.sql.Connection
import system.{PatchUtils, Patch}
import starling.richdb.{RichDB}
import starling.db.DBWriter
import starling.services.StarlingInit


class Patch7_UsdPosition_STR203 extends Patch{
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    PatchUtils.executeUpdateSQLFileFromClasspath(writer, "/schemaevolution/Patch7_STR203_AddUsdPositionColumn.sql")
  }

  def patchDescription() = "Add columns for VarReport for USD position"


}