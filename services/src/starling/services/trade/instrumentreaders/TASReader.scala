package starling.services.trade.instrumentreaders

import starling.daterange.{Month, SpreadPeriod, DateRangePeriod}
import starling.services.trade.ExcelRow
import starling.instrument.{TAS, FuturesCommoditySpread, FuturesCalendarSpread, Future}

class TASReader extends ExcelInstrumentReader {
  def canRead(row: ExcelRow) = row.instrumentType == TAS

  def create(row: ExcelRow) = {

    row.period match {
      case DateRangePeriod(period) => {
        val maturity = row.tradeDay // For now can only have the trade day as the maturity - all that's needed atm
        val market = row.futuresMarket
        val volume = row.volume
        TAS(market, period, maturity, volume)
      }
      case _ => throw new Exception("Invalid period for TAS: " + row.period + " ('" + row.rawPeriod + "')")
    }
  }
}