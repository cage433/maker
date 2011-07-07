package starling.pivot.pivotparsers

import starling.pivot.PivotParser
import starling.daterange.{TenorType, Day}

object DayPivotParser extends PivotParser {
  def parse(text:String) = {
    val day = Day.parse(text)
    (day, day.toShortString)
  }
}

object PeriodPivotParser extends PivotParser {
  def parse(text:String) = {
    val tenor = TenorType.parseTenor(text)
    (tenor, tenor.toString)
  }
}

class ValidMarketParser(allMarkets:Set[String]) extends PivotParser {
  def parse(text:String) = {
    val lowerCaseMarkets = allMarkets.map(_.trim.toLowerCase)
    if (lowerCaseMarkets(text.trim.toLowerCase)) {
      (text, text)
    } else {
      throw new Exception("Unknown market")
    }
  }
  override def acceptableValues = allMarkets
}