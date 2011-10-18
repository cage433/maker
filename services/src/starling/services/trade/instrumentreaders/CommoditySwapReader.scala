package starling.services.trade.instrumentreaders

import starling.daterange._
import starling.services.trade.ExcelRow
import starling.instrument.{SwapCalendarSpread, CommoditySwap}
import starling.market.{SingleIndex, PublishedIndex}

class CommoditySwapReader extends ExcelInstrumentReader {
  def canRead(row: ExcelRow) = row.instrumentType == CommoditySwap

  def create(row: ExcelRow) = {
    val index = row.index
    val volume = row.volume
    val strike = row.price
    val cleared = row.clearingHouse.nonEmpty
    val rule = row.pricingRule

    row.period match {
      case dr@DateRangePeriod(period) => {
        assert(period.firstDay.year > 2000, "period is invalid: " + dr)
        val swapPeriod = if(period.canSplitIntoMonths && period.toListOfMonths.size > 1) {
          // break period into monthly intervals
          val months: List[DateRange] = period.map(_.containingMonth).toList.distinct
          StripPeriod(months.head, months.last)
        } else {
          dr
        }
        CommoditySwap(index, strike, volume, swapPeriod, cleared, rule)
      }
      case s: StripPeriod => {
        val strip = stripToMonthStrip(s)
        require(strip.isDefined, "Period can not be split into months: " + s)
        CommoditySwap(index, strike, volume, strip.get, cleared, rule)
      }
      case period: SpreadPeriod => {
        index match {
          case si: SingleIndex => SwapCalendarSpread(si, strike, volume, period, cleared)
          case _ => throw new ExcelInstrumentReaderException("Can't have a swap spread on a multi index market: " + index)
        }

      }
      case p => throw new ExcelInstrumentReaderException("Invalid period for Swap: " + p)
    }
  }

  /**
   * Try to convert a strip into a month splittable daterange.
   */
  private def stripToMonthStrip(strip: StripPeriod): Option[StripPeriod] = (strip.first, strip.last) match {
    case (f: DateRangePeriod, l: DateRangePeriod) => {
      val dr = new SimpleDateRange(f.period.firstDay, l.period.lastDay)
      if (dr.canSplitIntoMonths) {
        val months: List[DateRange] = dr.toListOfMonths
        Some(StripPeriod(months.head, months.last))
      } else {
        None
      }
    }
    case _ => None
  }
}