package starling.schemaevolution

import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.db.DBWriter
import system.{PatchContext, Patch}


class Patch134_DropOldMarketDataTable extends Patch{
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    writer.update("drop table MarketData")
  }
  override def deferredReason(context: PatchContext) = context.dependsOn[Patch120_MigrateMarketDataToFasterSchema]
}