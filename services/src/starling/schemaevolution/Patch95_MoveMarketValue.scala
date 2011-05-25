package starling.schemaevolution

import system.Patch
import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.db.DBWriter
import starling.pivot.MarketValue
import starling.marketdata.PriceFixingsHistoryDataType
import starling.utils.sql.AnObject

class Patch95_MoveMarketValue extends Patch {
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    starling.inTransaction { writer =>
      writer.update("delete from MarketData where marketDataType = :t and marketDataSet = 'LimMetals'", Map("t"->AnObject(PriceFixingsHistoryDataType)) )
    }
  }
}