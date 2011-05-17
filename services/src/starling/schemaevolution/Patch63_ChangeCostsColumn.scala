package starling.schemaevolution

import starling.db.DBWriter
import system.{PatchUtils, Patch}
import starling.richdb.RichDB
import starling.services.StarlingInit

class Patch63_ChangeCostsColumn extends Patch {
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    PatchUtils.foreachTradeTable { table => {
      writer.update("ALTER TABLE dbo." + table + " alter column costs varchar(8000)")
    }}
  }

  def patchDescription() = "Change Costs Column"
}