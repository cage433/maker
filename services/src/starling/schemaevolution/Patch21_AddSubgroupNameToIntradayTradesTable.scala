package starling.schemaevolution

import system.Patch
import starling.db.DBWriter
import starling.richdb.RichDB
import starling.services.StarlingInit

/**
 * Need to differentiate between different users of the intraday trades upload, or
 * between different uploads by the same user.
 */
class Patch21_AddSubgroupNameToIntradayTradesTable extends Patch {
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    writer.update("alter table dbo.IntradayTrades add subgroupName varchar(255)")
  }

  def patchDescription = "Adds a subgroupName column to the intraday trades table."
}