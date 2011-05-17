package starling.schemaevolution

import starling.db.DBWriter
import starling.richdb.RichDB
import system.Patch
import starling.instrument.FuturesCalendarSpread
import starling.services.StarlingInit

class Patch75_ChangedFuturesSpreadTradeName extends Patch {
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    writer.update("update IntradayTrades set instrument = '" + FuturesCalendarSpread.name + "' where instrument = 'Futures Spread'")
  }

  def patchDescription = "Change instrument from 'Futures Spread' to 'Futures Calendar Spread'"
}