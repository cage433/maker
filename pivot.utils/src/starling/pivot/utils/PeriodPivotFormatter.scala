package starling.pivot.utils

import starling.pivot._
import starling.pivot.MonthFormat._
import starling.daterange.{DateRange, DateRangePeriod, Month}

object PeriodPivotFormatter extends PivotFormatter {
  def format(value:Any, formatInfo:ExtraFormatInfo) = {

    def match2(dateRange:DateRange) = {
      dateRange match {
        case m:Month => {
          val text = formatInfo.dateRangeFormat.monthFormat match {
            case Standard => m.toShortString
            case Short => m.toTinyString
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
      case other => DefaultPivotFormatter.format(other, formatInfo)
    }
  }
}