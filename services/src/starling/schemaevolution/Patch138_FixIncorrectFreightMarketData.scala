package starling.schemaevolution

import system.Patch
import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.db.DBWriter
import starling.tradestore.eai.EAITradeStore
import starling.market.Market
import starling.quantity.UOM

class Patch138_FixIncorrectFreightMarketData extends Patch {

  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) {
    val prices = """
    update mdv
    set uom = '%'
    from MarketDatavalue mdv
    inner join MarketDataExtendedKey mdek on mdek.id = mdv.extendedKey
    where marketDataType = 'Price'
    and uom = 'WSC/MT'
    """

    val fixings = """
    update mdv
    set uom = '%'
    from MarketDatavalue mdv
    inner join MarketDataExtendedKey mdek on mdek.id = mdv.extendedKey
    where marketDataType = 'PriceFixingsHistory'
    and uom = 'WSC/MT'
    """

    writer.update(prices)
    writer.update(fixings)
  }
}
