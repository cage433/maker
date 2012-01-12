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

object TimeOnlyTimestampPivotFormatter extends PivotFormatter {
  def format(value:Any, formatInfo:ExtraFormatInfo) = {
    new TableCell(value, value.asInstanceOf[Timestamp].timeStringWithSeconds)
  }
}
object TimestampPivotFormatter extends PivotFormatter {
  def format(value:Any, formatInfo:ExtraFormatInfo) = {
    value match {
      case t:Timestamp => {
        val shortText = t.toStringMinutes
        val longText =  shortText + t.toDateTime.toString(":ss.SSS z")
        new TableCell(value, shortText, longText = Some(longText))
      }
      case _ => new TableCell(value)
    }
  }
}

object TenorPivotParser extends PivotParser {
  protected def parseDefined(text: String, extraFormatInfo: ExtraFormatInfo) = text match {
    case Tenor.Parse(tenor) => (tenor, text)
  }
}