package starling.schemaevolution

import system.Patch
import starling.richdb.RichDB
import starling.daterange.Day
import starling.marketdata._
import starling.utils.sql.AnObject
import starling.db.DBWriter
import starling.services.StarlingInit
import collection.immutable.List
import starling.gui.api.{PricingGroup, MarketDataSelection}


class Patch86_MoveFixingsToSeparateRows extends Patch {
  import PricingGroup._

  override protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) {
    starling.inTransaction {
      writer => {
        writer.update("delete from MarketData where marketDataType = :t", Map("t"->AnObject(PriceFixingsHistoryDataType)) )
      }
    }

    starlingInit.marketDataStore.importFor(Day.today.previousWeekday, starlingInit.marketDataStore.marketDataSources.keys.toSeq : _*)
  }
}