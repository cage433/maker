package starling.services.trade.instrumentreaders

import starling.daterange._
import starling.instrument.AsianOption
import starling.services.trade.ExcelRow

class AsianOptionReader extends ExcelInstrumentReader {

  import ExcelInstrumentReader._

  def canRead(row: ExcelRow) = row.instrumentType == AsianOption

  def create(row: ExcelRow) = {
    val index = row.singleIndex
    val volume = row.volume
    val strike = row.strike
    val cp = row.callOrPut
    assert(row.exerciseType.isEmpty, "Asian options can't have a specified exercise type.")

    row.period match {
      case DateRangePeriod(period) => {
        val months = period.toListOfMonths
        val p = months match {
          case month :: Nil => DateRangePeriod(month)
          case _ => StripPeriod(months.head, months.last)
        }
        AsianOption(index, p, strike, volume, cp)
      }
      case s: StripPeriod => AsianOption(index, s, strike, volume, cp)
      case p => throw new ExcelInstrumentReaderException("Invalid period for Asian: " + p)
    }
  }
}