package starling.pivot.pivotparsers

import starling.daterange.{Month, TenorType, Day}
import starling.pivot.{ExtraFormatInfo, PivotParser}
import starling.pivot.utils.PeriodPivotFormatter
import starling.pivot.model.UndefinedValue

object DayPivotParser extends PivotParser {
  protected def parseDefined(text:String, extraFormatInfo:ExtraFormatInfo) = typedParse(text)

  def typedParse(text: String): (Day, String) = {
    val day = Day.parse(text)
    (day, day.toShortString)
  }
}

object ObservationDayPivotParser extends PivotParser {
  override protected def parsedUndefined(text: String) = UndefinedValue.parse(text)

  protected def parseDefined(text: String, extraFormatInfo: ExtraFormatInfo) = {
    val (d, s) = DayPivotParser.typedParse(text)
    if (d > Day.today) {
      throw new IllegalArgumentException("You can not specify a day in the future")
    }
    (d,s)
  }
}

object PeriodPivotParser extends PivotParser {
  protected def parseDefined(text:String, extraFormatInfo:ExtraFormatInfo) = {
    val tenor = TenorType.parseTenor(text)
    val label = PeriodPivotFormatter.format(tenor, extraFormatInfo).text
    (tenor, label)
  }
}