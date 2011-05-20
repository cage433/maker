package starling.schemaevolution

import system.Patch
import starling.db.DBWriter
import starling.richdb.RichDB
import starling.services.StarlingInit


class Patch96_RemoveTrinityMarketDataFromOilProd extends Patch{
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    starling.inTransaction {
      newWriter => {
        writer.update("delete from MarketData where marketDataKey like '%LBMA%'")
        writer.update("delete from MarketData where marketDataSet like 'Var:%'")
      }
    }

  }

}