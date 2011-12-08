package starling.schemaevolution

import system.Patch
import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.db.DBWriter


class Patch153_CNYForwardRatesAtCFETS extends Patch {
  protected def runPatch(ignore: StarlingInit, starling: RichDB, writer: DBWriter) = {
    writer.update("""UPDATE MarketDataExtendedKey SET observationTime = 'CFETS Publication Time'
      WHERE marketDataType = 'ForwardRate' AND marketDataKey LIKE '%CNY%'""")
  }
}