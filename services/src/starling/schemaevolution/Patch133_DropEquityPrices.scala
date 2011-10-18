package starling.schemaevolution

import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.db.DBWriter
import system.Patch


class Patch133_DropEquityPrices extends Patch{
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    writer.update(
    """
      delete from MarketDataValue where extendedKey in (
        select id from MarketDataExtendedKey where marketDataType like '%EQU%')

      delete from MarketDataExtendedKey where marketDataType like '%EQU%'
    """
    )
  }
}
