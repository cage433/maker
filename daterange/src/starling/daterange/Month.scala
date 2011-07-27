package starling.daterange

import scala.util.matching.Regex
import starling.daterange.DayOfWeek._

/**
 * calendar month type.
 *
 * note that +m+, the month value, is indexed from 1=jan, not zero.
 */
case class Month(y : Int, m : Int) extends DateRange {
  require(m >= 1 && m <= 12, "Invalid month number:" + m)

  override def toString = toPrintableForm

  def toPrintableForm = Month.months(m - 1).capitalize + " " + y

  override def toShortString = Month.months(m - 1).capitalize.take(3) + " " + y

  def min(that : Month) : Month = if (this < that) this else that

  def max(that : Month) : Month = if (this > that) this else that

  /**
   * inclusive of first and last month
   */
  def upto(lastMonth: Month): List[Month] = {
    assert(this <= lastMonth, "Month is before this")
    if (this == lastMonth)
      List(this)
    else
      this :: (next upto lastMonth)
  }

  @transient override lazy val firstDay : Day = Day(y, m, 1)
  @transient override lazy val lastDay : Day = next.firstDay - 1

  private def readResolve() : Object = Month(y, m)

  def next : Month = if (m == 12) Month(y + 1, 1) else Month(y, m + 1)
	def previous : Month = if (m == 1) Month(y - 1, 12) else Month(y, m - 1)

	def + (i : Int) : Month = {
    def mod(n : Int, m : Int) = {
      val x = n % m
      if (x < 0)
        x + m
      else
        x
    }
    val monthOffset = i + m - 1
    if (monthOffset >= 0)
      Month(y + monthOffset / 12, mod(monthOffset, 12) + 1)
    else
      Month(y + (monthOffset + 1) / 12 - 1, mod(monthOffset, 12) + 1)
	}
  def -(i : Int) = this + (-i)

  def - (that : Month) : Int = m - that.m + 12 * (y - that.y)

  def / (that: Month): Spread[Month] = Spread(this, that)

  def tenor = Some(Month)

  def firstTuesday: Day = first(DayOfWeek.tuesday)
  def firstWednesday: Day = first(DayOfWeek.wednesday)
  def thirdWednesday = firstWednesday + 14
  def lastSunday = last(DayOfWeek.sunday)

  def first(dayOfWeek: DayOfWeek) = firstDay + ((7 + (dayOfWeek - firstDay.dayOfWeek)) % 7)
  def last(dayOfWeek: DayOfWeek) = lastDay - ((7 + (lastDay.dayOfWeek - dayOfWeek)) % 7)

  def forDay(d:Int) = Day(y, m, d)

  def toListOfMonths = List(this)
}

object Month extends TenorType with Serializable {
  type T = Month

	implicit object ordering extends Ordering[Month] {
		def compare(lhs : Month, rhs : Month) : Int = DateRange.ordering.compare(lhs, rhs)
	}
	
	val months: List[String] = List(
			"JANUARY", "FEBRUARY", "MARCH", "APRIL", "MAY", "JUNE",
			"JULY", "AUGUST", "SEPTEMBER", "OCTOBER", "NOVEMBER", "DECEMBER"
	)
	val shortMonths: List[String] = months.map(_.substring(0, 3))

  def shortMonthName(monthNumber : Int) = shortMonths(monthNumber - 1)
  /* hack to allow Month to be an instance of Ordered, even though DateRange is also and Ordered isn't
   * covariant. see http://stackoverflow.com/questions/1818777/extend-scala-class-that-extends-ordered
   * for more information.
   */
  implicit def orderedDR2orderedMonth[Month <: DateRange with Ordered[DateRange]](m : Month) : Ordered[Month] = m.asInstanceOf[Ordered[Month]]

  lazy val monthRegex: Regex = new Regex("(" + (months ++ shortMonths).mkString("", "|", "") + """)\s*[ -]?\s*(.*)""")
  lazy val monthNumRegex: Regex = """([0-9]{2})/([0-9]{2,4})""".r

  /**
   * parses the following formats:
   * Jan-10
   * Jan 10
   * January 10
   * Jan 2010
   * January 2010
   * F0
   * F10
   * F2010
   */
	def parse(text : String): Month = {
		text.toUpperCase match {
      case monthNumRegex(monthNum, yearNum) => {
        val y = Year.parse(yearNum)
        Month(y.yearNumber, monthNum.toInt)
      }
		  case monthRegex(monthName, yearStr) =>
        val y = Year.parse(yearStr)
        val m = shortMonths.indexOf(monthName.substring(0, 3)) + 1
        Month(y.yearNumber, m)
      case	_ => {
        ReutersDeliveryMonthCodes.parse(text) match {
          case Some(month) => month
          case None => throw new IllegalStateException("Can't parse month text " + text)
        }
      }
		}
	}

  override def toString = "Month"
  override def containing(d : Day)= d.containingMonth


  def add(n: Int, from: Month) = from + n
  def difference(to: Month, from: Month) = to - from

  /** Breaks a list of months into contuguous groups. Method is public so 
      it can be tested
  */
  def contiguousMonths(months : List[Month], acc : List[List[Month]] = Nil) : List[List[Month]] = {
    if (months.isEmpty)
      return (acc.map(_.reverse)).reverse
    val nextMonth = months.head
    acc match {
      case (mth::rest1) :: rest2 if (mth == nextMonth - 1) => 
        contiguousMonths(months.tail, (nextMonth :: mth :: rest1) :: rest2)
      case _ => contiguousMonths(months.tail, List(nextMonth) :: acc)
    }
  }

  def shortName = "M"
}

object ReutersDeliveryMonthCodes {
  import Month._

  lazy val reutersMonthYearRegex = ("(?i)(" + codes.keys.mkString("|") + """)[ -]?(.+)""").r
  lazy val reutersYearMonthRegex = ("(\\d+)(" + codes.keys.mkString("|") + ")").r

  /**
   * Parses months formatted as:
   * "F0" = Jan 10
   * or
   * "F10" = Jan 10
   */
  def parse(text : String): Option[Month] = {
		text.toUpperCase match {
      case reutersMonthYearRegex(code, yearStr) => codes.get(code).map {
        monthNum => Month(Year.parse(yearStr).yearNumber, monthNum)
      }
      case reutersYearMonthRegex(yearStr, code) => codes.get(code).map {
        monthNum => Month(Year.parse(yearStr).yearNumber, monthNum)
      }
      case _ => None
    }
  }

  // http://en.wikipedia.org/wiki/Reuters_Instrument_Code
  lazy val codes = Map(
    "F" -> 1,
    "G" -> 2,
    "H" -> 3,
    "J" -> 4,
    "K" -> 5,
    "M" -> 6,
    "N" -> 7,
    "Q" -> 8,
    "U" -> 9,
    "V" -> 10,
    "X" -> 11,
    "Z" -> 12
    )
}

