package starling.pivot.pivotparsers

import starling.daterange.{Month, TenorType, Day}
import starling.pivot.{ExtraFormatInfo, PivotParser}
import starling.pivot.utils.PeriodPivotFormatter

object DayPivotParser extends PivotParser {
  def parse(text:String, extraFormatInfo:ExtraFormatInfo) = {
    val day = Day.parse(text)
    (day, day.toShortString)
  }
}

object PeriodPivotParser extends PivotParser {
  def parse(text:String, extraFormatInfo:ExtraFormatInfo) = {
    val tenor = TenorType.parseTenor(text)
    val label = PeriodPivotFormatter.format(tenor, extraFormatInfo).text
    (tenor, label)
  }
}

class ValidMarketParser(allMarkets:Set[String]) extends PivotParser {
  def parse(text:String, extraFormatInfo:ExtraFormatInfo) = {
    val lowerCaseMarkets = allMarkets.map(_.trim.toLowerCase)
    if (lowerCaseMarkets(text.trim.toLowerCase)) {
      (text, text)
    } else {
      throw new Exception("Unknown market")
    }
  }
  override def acceptableValues = allMarkets
}