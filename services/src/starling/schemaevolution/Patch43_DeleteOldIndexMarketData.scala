package starling.schemaevolution

import system.Patch
import starling.db.DBWriter
import starling.richdb.RichDB
import starling.services.StarlingInit

class Patch43_DeleteOldIndexMarketData  extends Patch {
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    starling.inTransaction {
      newWriter => {
        writer.update("delete FROM MarketData where marketDataKey like '%>WTI 1st Pos<%'")
        writer.update("delete FROM MarketData where marketDataKey like '%>WTI 2nd Pos<%'")
      }
    }
  }

  def patchDescription = "delete market data for removed indexes"
}