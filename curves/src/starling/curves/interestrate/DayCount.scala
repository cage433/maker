package starling.curves.interestrate

import starling.daterange.{DateRange, Day}

/**
 * Trait implemented by different daycount conventions, such as 30/360, ACT/365 etc
 * Calculates the time in years between any two days
 */
trait DayCount{
  def assertDaysInOrder(start : Day, end : Day){
    assert (end >= start, "Stary day " + start + " is after end " + end)
  }
  def factor(start : Day, end : Day) : Double
  def factor(period : (Day, Day)) : Double = factor(period._1, period._2)
  def factor(period : DateRange) : Double = factor(period.firstDay, period.lastDay)

}

object DayCount{
  def fromTrinityCode(dayCount : String) : DayCount = {
    dayCount.toUpperCase match {
      case "ACT'L/360" => DayCountActual360
      case "ACT'L/365" => DayCountActual365
      case "ACT'L/ACT'L" => DayCountActualActual
      case "30/360" => DayCount30_360
      case _ => throw new IllegalStateException("Unrecognixed day count " + dayCount)
    }
  }
}

/**
 * For ACT/ACT the rule is
 *
 * factor = days not in leap year   +     Days in leap year
 *          --------------------          ----------------
 *                  365                         366
 *
 * where the first day is included, the last day is excluded
 */

object DayCountActualActual extends DayCount{
  def factor(start: Day, end: Day) : Double = {
    assertDaysInOrder(start, end)
    var years = 0.0
    if (start == end)
      return 0.0
    var d1 = start
    var d2 = d1
    while (d2 < end){
      val year = d1.containingYear
      d2 = (year.lastDay + 1) min end
      years += (d2 - d1) / year.daysInYear.asInstanceOf[Double]
      d1 = d2
      d2 = d1
    }
    years
  }
  override def toString = "ACT'L/ACT'L"
}


object DayCountActual360 extends DayCount{
  def factor(start: Day, end: Day) = {
    assertDaysInOrder(start, end)
    (end - start) / 360.0
  }
  override def toString = "ACT'L/360"
}

object DayCountActual365 extends DayCount{
  def factor(start: Day, end: Day) = {
    assertDaysInOrder(start, end)
    (end - start) / 365.0
  }
  override def toString = "ACT'L/365"
}

/**
 * We use the 30E/360 convention. This is because
 *  a) It's really simple
 *  b) Trinity has man 30/360 basis types in its basis table - which specific
 *     one it means by 30/360 is ambiguous
 *  c) The difference isn't a core part of Trafigura's I'm guessing
 * this is what Trinity means by this day count, as its basis table
 * also contains 30u/360, which is a term used for the american style 30/360
 */
object DayCount30_360 extends DayCount{
  def factor(start: Day, end: Day) = {
    assertDaysInOrder(start, end)

    def dmy(day : Day) = (day.dayNumber, day.month, day.year)
    var (d1, m1, y1) = dmy(start)
    var (d2, m2, y2) = dmy(end)

    if (d1 == 31)
      d1 = 30

    if (d2 == 31){
      d2 = 30
    }

    (360 * (y2 - y1) + 30 * (m2 - m1) + d2 - d1) / 360.0
  }

  override def toString = "30/360"
}

