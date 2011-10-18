package starling.services.trade.instrumentreaders

import starling.quantity.Quantity
import starling.daterange.{Month, SpreadPeriod, DateRangePeriod}
import starling.services.trade.ExcelRow
import starling.instrument.{FuturesCommoditySpread, FuturesCalendarSpread, Future}

class FutureReader extends ExcelInstrumentReader {
  def canRead(row: ExcelRow) = row.instrumentType == Future

  def create(row: ExcelRow) = {
    val volume = row.volume

    row.period match {
      case DateRangePeriod(period: Month) if row.isSpreadMarket => {
        assert(period.y > 2000, "period is invalid: " + period)
        val market = row.futuresSpreadMarket
        row.prices match {
          case firstPrice :: lastPrice :: Nil => new FuturesCommoditySpread(market, period, firstPrice, lastPrice, volume)
          case spreadPrice :: Nil => new FuturesCommoditySpread(market, period, spreadPrice, volume)
          case _ => throw new ExcelInstrumentReaderException("Invalid prices specified for Future: " + row.prices)
        }
      }
      case DateRangePeriod(period) if row.isSpreadMarket => {
        throw new ExcelInstrumentReaderException("Invalid period for a crack market: " + period)
      }
      case DateRangePeriod(period) => {
        assert(period.firstDay.year > 2000, "period is invalid: " + period)
        val strike = row.price
        val market = row.futuresMarket
        Future(market, period, strike, volume)
      }
      case s@SpreadPeriod(first: Month, last: Month) => {
        assert(first.y > 2000, "period is invalid: " + s)
        val market = row.futuresMarket
        row.prices match {
          case firstPrice :: lastPrice :: Nil => new FuturesCalendarSpread(market, first, last, firstPrice, lastPrice, volume)
          case spreadPrice :: Nil => new FuturesCalendarSpread(market, first, last, spreadPrice, volume)
          case _ => throw new ExcelInstrumentReaderException("Invalid prices specified for Future: " + row.prices)
        }
      }
      case _ => throw new Exception("Invalid period for this future: " + row.period + " ('" + row.rawPeriod + "')")
    }
  }
}