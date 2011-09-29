package starling.schemaevolution

import system.Patch
import starling.db.DBWriter
import starling.richdb.RichDB
import starling.services.StarlingInit


class Patch92_RealtimeMarketDataNullObservationDay extends Patch {
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    writer.update("alter table dbo.MarketData alter column observationDay datetime null")
  }
}