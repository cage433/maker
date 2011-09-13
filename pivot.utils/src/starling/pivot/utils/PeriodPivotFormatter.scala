package starling.pivot.utils

import starling.pivot._
import starling.pivot.MonthFormat._
import starling.daterange.{SpreadPeriod, DateRange, DateRangePeriod, Month}

object PeriodPivotFormatter extends PivotFormatter {
  def format(value:Any, formatInfo:ExtraFormatInfo) = {

    def match2(dateRange:DateRange) = {
      dateRange match {
        case m:Month => {
          val text = formatInfo.dateRangeFormat.monthFormat match {
            case Standard => m.toShortString
            case StandardCapitalised => m.toShortStringCapitalised
            case Short => m.toTinyString
            case ShortCapitalised => m.toTinyStringCapitalised
            case ShortDash => m.toTinyStringDash
            case ShortDashCapitalised => m.toTinyStringDashCapitalised
            case Numeric => m.toNumericString
            case Reuters => m.toReutersString
          }
          TableCell(m, text)
        }
        case other => DefaultPivotFormatter.format(other, formatInfo)
      }
    }

    value match {
      case DateRangePeriod(period) => match2(period)
      case dateRange:DateRange => match2(dateRange)
      case sp@SpreadPeriod(dr1, dr2) => {
        val t1 = match2(dr1)
        val t2 = match2(dr2)
        t1.copy(value = sp, text = (t1.text + "/" + t2.text))
      }
      case other => DefaultPivotFormatter.format(other, formatInfo)
    }
  }
}