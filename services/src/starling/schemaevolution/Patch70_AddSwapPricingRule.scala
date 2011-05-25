package starling.schemaevolution

import system.Patch
import starling.db.DBWriter
import starling.richdb.RichDB
import starling.eai.EAIAutoImport
import starling.services.StarlingInit

class Patch70_AddSwapPricingRule extends Patch {
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    val tables = List("IntradayTrades", "EAITrade", "GalenaTrade", "SoftmarTrade", "TrinityTrade")
    writer.update("truncate table EAITrade")
    writer.update("truncate table ClosedDesks")
    for (table <- tables) {
      writer.update("alter table " + table + " add PricingRule varchar(25) default null")
      writer.update("update " + table + " set PricingRule = 'Common' where instrument = 'Commodity Swap'")
    }
  }

  def patchDescription = "Add column for swap pricing rule"
}