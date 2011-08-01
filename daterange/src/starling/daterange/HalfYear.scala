package starling.daterange

import java.lang.String

/**
 * half a calendar year.
 *
 * can be either the front half or the back half
 */
case class HalfYear(year : Year, backHalf : Boolean) extends DateRange {
  def this(y : Int, fb : FrontOrBack.Value) = this(new Year(y), fb != FrontOrBack.Front)

  override def firstDay = if(!backHalf) year.firstDay else Day(year.yearNumber, 7, 1)
  override def lastDay = if(backHalf) year.lastDay else Day(year.yearNumber, 6, 30)
  
  override def compare(that : DateRange) = that match {
    case thatHY : HalfYear => {
      val compareYear = year compare thatHY.year
      if (compareYear == 0) backHalf compare thatHY.backHalf else compareYear
    }
    case _ => super.compare(that)
  }
  
  override def toString = {
    val fbAsString = if (backHalf) " BACK" else " FRONT"
    year.toString + fbAsString
  }

  def intervalNumber = (year.yearNumber - 1) * 2 + (if (backHalf) 1 else 0)

  def tenor = Some(HalfYear)

  def +(i : Int) : HalfYear = {
    val increment = if (backHalf) i + 1 else i
    new HalfYear(year + increment / 2, (increment % 2) == 1)
  }
  def -(that : HalfYear) : Int = {
    val correction = (if (backHalf) 1 else 0) - (if (that.backHalf) 1 else 0)
    2 * (year - that.year) + correction
  }

  def toListOfYears = throw new Exception("Half year " + this + "Can't be broken into years")

  def toListOfMonths = (1 to 6).toList.map(v => Month(year.yearNumber, if(backHalf) 6 + v else v))
}

object HalfYear extends TenorType {
  type T = HalfYear

  implicit def orderedHack[HalfYear <: DateRange with Ordered[DateRange]](hm : HalfYear) : Ordered[HalfYear] = hm.asInstanceOf[Ordered[HalfYear]]

  def HalfWayPoint(year: Int): Day = Day(year, 7, 1)

  // half-year which contains the day.
  def containing(d : Day) : HalfYear = {
    new HalfYear(d.containingYear, d >= HalfWayPoint(d.year))
  }

  def add(n: Int, from: HalfYear) = from + n
  def difference(to: HalfYear, from: HalfYear) = to - from

  val ParserRegex1 = """(?i)(.*) (BACK|FRONT)""".r
  val ParserRegex2 = """(?i)([12]{1})h[ -]?(.*)""".r
  val ParserRegex3 = """(?i)h([12]{1})[ -]?(.{2,})""".r

  def parse(text : String) = {
    text match {
      case ParserRegex1(yearString, backFront) =>
        HalfYear(Year.parse(yearString), backFront.toLowerCase == FrontOrBack.Back.toString.toLowerCase)
      case ParserRegex2(half, yearString) =>
        HalfYear(Year.parse(yearString), half.toInt == 2)
      case ParserRegex3(half, yearString) =>
        HalfYear(Year.parse(yearString), half.toInt == 2)
      case _ => throw new IllegalStateException("Can't parse half-year from `" + text + "'")
    }
  }

  override def toString = "HalfYear"

  override def shortName = "HY"
}
