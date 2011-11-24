package starling.pivot.pivotparsers

import starling.daterange.{Month, TenorType, Day}
import starling.pivot.{ExtraFormatInfo, PivotParser}
import starling.pivot.utils.PeriodPivotFormatter
object DayPivotParser extends PivotParser {
  def parse(text:String, extraFormatInfo:ExtraFormatInfo) = typedParse(text)

  def typedParse(text: String): (Day, String) = {
    val day = Day.parse(text)
    (day, day.toShortString)
  }
}

object ObservationDayPivotParser extends PivotParser {
  def parse(text: String, extraFormatInfo: ExtraFormatInfo) = {
    DayPivotParser.typedParse(text).ensuring(_._1 <= Day.today)
  }
}

object PeriodPivotParser extends PivotParser {
  def parse(text:String, extraFormatInfo:ExtraFormatInfo) = {
    val tenor = TenorType.parseTenor(text)
    val label = PeriodPivotFormatter.format(tenor, extraFormatInfo).text
    (tenor, label)
  }
}

class SpecifiedValuesParser(allAcceptableValues:Set[String]) extends PivotParser {
  def parse(text:String, extraFormatInfo:ExtraFormatInfo) = {
    val lowerCaseValues = allAcceptableValues.map(_.trim.toLowerCase)
    if (lowerCaseValues(text.trim.toLowerCase)) {
      (text, text)
    } else {
      throw new Exception("Unknown value")
    }
  }
  override def acceptableValues = allAcceptableValues
}