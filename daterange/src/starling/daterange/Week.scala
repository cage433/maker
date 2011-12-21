package starling.daterange

import java.lang.String
import util.matching.Regex

/**
 * ISO 8601 week.
 *
 * note week numbers are 1-indexed.
 */
case class Week(year : Int, isoWeekNumber : Int) extends DateRange {
  require(isoWeekNumber > 0 && isoWeekNumber <= Week.numWeeksInYear(year),
    "Invalid ISO week number")

  override def firstDay = Week.yearStart(year) + (isoWeekNumber - 1) * 7
  override def lastDay = Week.yearStart(year) + isoWeekNumber * 7 - 1
  override def toString = year + "W" + isoWeekNumber
  override def compare(that : DateRange) = that match {
    case thatWeek : Week => {
      val yearCompare = year.compare(thatWeek.year)
      if (yearCompare == 0) isoWeekNumber.compare(thatWeek.isoWeekNumber) else yearCompare
    }
    case _ => super.compare(that)
  }
  
  def day(ofWeek : DayOfWeek) : Day = firstDay + ofWeek.number

  def tenor = Some(Week)

  def +(n : Int) = Week.containing(firstDay + n * 7)
  def -(n: Int) = this + (-n)
  def -(that : Week) = (firstDay - that.firstDay) / 7

  def toListOfMonths = throw new Exception("Week " + this + " can't be broken into months")
}

object Week extends TenorType {
  type T = Week

  /**
   * whether the year number is a short ISO year (52 weeks) or a long year (53 weeks)
   *
   * direct implementation of the algorithm here:
   * http://www.phys.uu.nl/~vgent/calendar/isocalendar.htm
   *
   * the following code assumes that y > 200 to avoid doing the floor() operations
   * which aren't required in most (modern) cases.
   */
  def isShortYear(y : Int) : Boolean = {
    val gy = (y - 100) / 400 - (y - 102) / 400
    val hy = (y - 200) / 400 - (y - 199) / 400
    val fy = 5 * y + 12 - 4 * (y / 100 - y / 400) + gy + hy
    (fy % 28) > 4
  }

  def numWeeksInYear(y : Int) : Int = if (isShortYear(y)) 52 else 53

  def yearStart(y : Int) : Day = {
    val fourthJan = Day(y, 1, 4)
    fourthJan - fourthJan.dayOfWeek.number
  }

  /**
   * the week containing the given day.
   */
  def containing(d : Day) : Week = {
    // this is teh uglies, but i'm not sure there's any better way of expressing it
    val yearStartDay = yearStart(d.year)
    if (d < yearStartDay)
      new Week(d.year - 1, (d - yearStart(d.year - 1)) / 7 + 1)
    else {
      val weekNum = (d - yearStartDay) / 7 + 1
      if (weekNum > numWeeksInYear(d.year)) {
        val nextYearStartDay = yearStart(d.year + 1)
        new Week(d.year + 1, (d - nextYearStartDay) / 7 + 1)
      } else
        new Week(d.year, weekNum)
    }
  }

  def add(n: Int, from: Week) = from + n
  def difference(to: Week, from: Week) = to - from

  val weekRegex = new Regex("(?i)(\\d{2,4})w(\\d{2})")

  def parse(s : String) = s match {
    case weekRegex(yString, wString) => {
      val year = Integer.parseInt(yString)
      val week = Integer.parseInt(wString)
      if (year < 80)
        Week(2000 + year, week)
      else if (year < 100)
        Week(1900 + year, week)
      else
        Week(year, week)
    }
    case _ => throw new IllegalStateException("Can't parse a week from `" + s + "'")
  }

  override def toString = "Week"

  override def shortName = "W"
}