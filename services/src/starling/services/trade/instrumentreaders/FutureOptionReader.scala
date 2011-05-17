package starling.services.trade.instrumentreaders

import starling.quantity.Quantity
import starling.daterange.{Month, SpreadPeriod, DateRangePeriod}
import starling.instrument.{FuturesOption, FuturesCalendarSpread, Future}
import starling.models.American
import starling.services.trade.ExcelRow

class FutureOptionReader extends ExcelInstrumentReader {
  def canRead(row: ExcelRow) = row.instrumentType == FuturesOption

  def create(row: ExcelRow) = {
    val market = row.futuresMarket
    val volume = row.volume
    val strike = row.strike
    val cp = row.callOrPut
    val ex = row.exerciseType.getOrElse(American) // default to American

    row.period match {
      case DateRangePeriod(period) => {
        FuturesOption(market, market.optionExpiry(period), period, strike, volume, cp, ex)
      }
      case p => throw new ExcelInstrumentReaderException("Invalid period for futures option: " + p)
    }
  }
}