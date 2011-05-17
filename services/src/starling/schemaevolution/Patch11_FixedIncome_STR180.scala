package starling.schemaevolution

import java.sql.Connection
import system.{PatchUtils, Patch}
import starling.richdb.{RichDB}
import starling.db.DBWriter
import starling.services.StarlingInit


class Patch11_FixedIncome_STR180 extends Patch{
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    PatchUtils.executeUpdateSQLFileFromClasspath(writer, "/schemaevolution/Patch11_STR180_AddFixedIncomeTrades.sql")
  }

  def patchDescription() = "Add columns for fixed income trades"
}