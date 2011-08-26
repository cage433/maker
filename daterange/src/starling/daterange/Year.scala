package starling.daterange

import java.lang.String
import util.matching.Regex

/**
 * calendar year.
 */
case class Year(yearNumber : Int) extends DateRange {
  override def firstDay = Day(yearNumber, 1, 1)

  override def lastDay = Day(yearNumber, 12, 31)

  override def compare(that : DateRange) : Int = that match {
    case yearThat : Year => yearNumber.compare(yearThat.yearNumber)
    case _ => super.compare(that)
  }

  override def equals(p1: Any) = p1 match {
    case y : Year => y.yearNumber == yearNumber
    case _ => false
  }

  def previous : Year = Year(yearNumber - 1)
  def next : Year = Year(yearNumber + 1)

  def +(n : Int) : Year = new Year(yearNumber + n)
  def -(n : Int) : Year = this + (-n)
  def -(that : Year) : Int = yearNumber - that.yearNumber

  def tenor = Some(Year)

  def isLeapYear : Boolean = {
    if (yearNumber % 400 == 0)
      true
    else if (yearNumber % 100 == 0)
      false
    else if (yearNumber % 4 == 0)
      true
    else
      false
  }

  def daysInYear = if (isLeapYear) 366 else 365

  def quarters : List[Quarter] = (1 to 4).map(Quarter(yearNumber, _)).toList

  override def toString = "Y" + yearNumber

  def toListOfMonths = (1 to 12).toList.map(Month(yearNumber, _))
}

object Year extends TenorType {
  type T = Year

	implicit object ordering extends Ordering[Year] {
		def compare(lhs : Year, rhs : Year) : Int = DateRange.ordering.compare(lhs, rhs)
	}
	
  // hack to allow Ordered covariance. see comment in Day's companion object.
  implicit def orderedHack[Year <: DateRange with Ordered[DateRange]](y : Year) : Ordered[Year] = y.asInstanceOf[Ordered[Year]]

  def containing(d : Day) = new Year(d.year)

  def add(n: Int, from: Year) = Year(from.yearNumber + n)
  def difference(to: Year, from: Year) = to.yearNumber - from.yearNumber

  val oneDigitYearRegex = new Regex("(?i)(cal|y)?[ -]?(\\d{1})")
  val twoDigitYearRegex = new Regex("(?i)(cal|y)?[ -]?(\\d{2})")
  val fourDigitYearRegex = new Regex("(?i)(cal|y)?[ -]?(\\d{4})")

  def parse(s : String) = {
    s match {
      case oneDigitYearRegex(calText, back) => {
        Year(2010 + back.toInt)
      }
      case twoDigitYearRegex(calText, back) => {
        val y = Integer.parseInt(back)
        if (y < 80)
          Year(y + 2000)
        else
          Year(y + 1900)
      }
      case fourDigitYearRegex(calText, num) => Year(Integer.parseInt(num))
      case _ => throw new IllegalStateException("Can't parse year from `" + s + "'")
    }
  }

  override def toString = "Year"

  override def shortNames = List("Y", "CAL")
}
