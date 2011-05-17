package starling.daterange

import java.lang.String
import util.matching.Regex

/**
 * a quarter year, three calendar months.
 */
case class Quarter(year : Int, number : Int) extends DateRange {
  require(number > 0 && number <= 4, "Invalid quarter number")

  override def firstDay = Day(year, (number - 1) * 3 + 1, 1)
  override def lastDay = Month(year, number * 3).lastDay
  override def compare(that : DateRange) = that match {
    case thatQuarter : Quarter => {
      val yearCompare = year.compare(thatQuarter.year)
      if (yearCompare == 0) number.compare(thatQuarter.number) else yearCompare
    }
    case _ => super.compare(that)
  }

  override def toString = "" + year + "Q" + number

  def tenor = Some(Quarter)

  def +(n : Int) = {
    val ordinal : Int = 4 * year + n + number - 1
    Quarter(ordinal / 4, ordinal % 4 + 1)
  }
  def -(that : Quarter) = 4 * (year - that.year) + number - that.number

  def toListOfMonths = (1 to 3).toList.map{ i => Month(year, (3 * (number - 1) + i))}
}

object Quarter extends TenorType {
  type T = Quarter
	
	implicit object ordering extends Ordering[Quarter] {
		def compare(lhs : Quarter, rhs : Quarter) : Int = DateRange.ordering.compare(lhs, rhs)
	}
	
  // hack to allow Ordered covariance. see comment in Day's companion object.
  implicit def orderedHack[Quarter <: DateRange with Ordered[DateRange]](q : Quarter) : Ordered[Quarter] = q.asInstanceOf[Ordered[Quarter]]

  def containing(d : Day) : Quarter = {
    Quarter(d.year, (d.month - 1) / 3 + 1)
  }

  def add(n: Int, from: Quarter) = from + n
  def difference(to: Quarter, from: Quarter) = to - from

  val yearFirstQuarterRegex = new Regex("(?i)(.*)[ -]?q([1-4])")
  val yearLastQuarterRegex = new Regex("(?i)q([1-4])[ -]?(.*)")

  def parse(s : String) = s match {
    case yearFirstQuarterRegex(yString, qString) => {
      val year = Year.parse(yString)
      val quarter = Integer.parseInt(qString)
      Quarter(year.yearNumber, quarter)
    }
    case yearLastQuarterRegex(qString, yString) => {
      val year = Year.parse(yString)
      val quarter = Integer.parseInt(qString)
      Quarter(year.yearNumber, quarter)
    }
    case _ => throw new IllegalStateException("Can't parse quarter from `" + s + "'")
  }

  override def toString = "Quarter"

  def shortName = "Q"
}