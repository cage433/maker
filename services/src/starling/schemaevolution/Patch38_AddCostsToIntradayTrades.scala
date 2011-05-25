package starling.schemaevolution

import system.Patch
import starling.db.DBWriter
import starling.richdb.RichDB
import starling.services.StarlingInit

class Patch38_AddCostsToIntradayTrades extends Patch {
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    starling.inTransaction {
      newWriter => {
        writer.update("alter table IntradayTrades add costs text null")
      }
    }
  }

  def patchDescription = "Add columns needed for Costs to IntradayTrades table"
}