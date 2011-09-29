package starling.pivot.utils

import starling.pivot._
import starling.pivot.MonthFormat._
import starling.daterange._

object PeriodPivotFormatter extends PivotFormatter {
  def format(value:Any, formatInfo:ExtraFormatInfo):TableCell = {

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
      case sp@StripPeriod(p1, p2) => {
        val tc1 = PeriodPivotFormatter.format(p1, formatInfo)
        val tc2 = PeriodPivotFormatter.format(p2, formatInfo)
        tc1.copy(value = sp, text = (tc1.text + "-" + tc2.text))
      }
      case other => DefaultPivotFormatter.format(other, formatInfo)
    }
  }
}

object TimestampPivotFormatter extends PivotFormatter {
  def format(value:Any, formatInfo:ExtraFormatInfo) = {
    value match {
      case t:Timestamp => {
        val monthText = PeriodPivotFormatter.format(t.month, formatInfo).text
        val dayString = {
          val zz = t.day.dayNumber.toString
          if (Character.isDigit(monthText(0))) {
            zz + "/"
          } else {
            zz
          }
        }
        val textToUse = dayString + monthText + " " + t.timeStringWithMilliSeconds
        new TableCell(value, textToUse)
      }
      case _ => new TableCell(value)
    }
  }
}