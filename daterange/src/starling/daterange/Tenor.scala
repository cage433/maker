package starling.daterange

import starling.utils.Pattern._
import starling.utils.ImplicitConversions._
import starling.utils.Int


case class Tenor(tenorName: String, value: Int) extends Ordered[Tenor] {
  lazy val tenorType = TenorType.typesByShortName(tenorName)

  def compare(that: Tenor) = indexOf(tenorName).compare(indexOf(that.tenorName)) match {
    case 0 => value.compare(that.value)
    case other => other
  }

  def format(pattern: String) = this match {
    case Tenor.ON   => "ON"
    case Tenor.SN   => "SN"
    case Tenor.CASH => "CASH"
    case _          => pattern.format(value, tenorName)
  }

  override def toString = format("%d%s")

  def +(day: Day): Day = this match {
    case Tenor.ON => day + 1
    case Tenor.SN => day + 1
    case other => other.tenorType match {
      case Day       => day + value
      case Week      => day + (7 * value)
      case HalfMonth => day + (15 * value)
      case Month     => day.addMonths(value)
      case Quarter   => day.addMonths(3 * value)
      case HalfYear  => day.addMonths(6 * value)
      case Year      => {
        val theYear: Int = day.year + value
        val theMonth: Int = day.month
        val theDay: Int = day.dayNumber
        val x: Day = Day(theYear, theMonth, theDay)
        x
      }
    }
  }

  def toDateRange(day: Day) = tenorType.containing(day) + value

  private def indexOf(tenor: String) = TenorType.ALL_IN_ORDER.indexOf(tenor)
}

object Tenor {
  val Parse = Extractor.regex[Tenor]("""(\d+)(\w+)""") {
    case List(Int(value), TenorType.FromShortName(tenorType)) => Tenor(tenorType, value)
  }.orElse(_.partialMatch {
    case "ON"     => Tenor.ON
    case "SN"     => Tenor.SN
    case "CASH"   => Tenor.CASH
    case "CURMON" => Tenor(Month, 0)
    case "CURQ"   => Tenor(Quarter, 0)
  })

  def parse(any: Any): Tenor = Parse.unapply(any.toString).getOrElse(throw new Exception("Could not parse: " + any))

  /**
   * Value doesn't mean anything outside the context of a particular index. It is used just for
   * sorting tenors in a reasonable way
   */
  def apply(tenorType: TenorType, value: Int): Tenor = Tenor(tenorType.shortName, value)
  def many(tenorType: TenorType, values: Int*): List[Tenor] = values.map(apply(tenorType, _)).toList
  def many(name: String, values: Int*): List[Tenor] = values.map(Tenor(name, _)).toList

  val ON          = Tenor(Day, 0)   // Overnight
  val SN          = Tenor(Day, 2)   // Spot Next
  val CASH        = Tenor(Month, 0)
  val OneDay      = Tenor(Day, 1)
  val OneWeek     = Tenor(Week, 1)
  val TwoWeeks    = Tenor(Week, 2)
  val OneMonth    = Tenor(Month, 1)
  val TwoMonths   = Tenor(Month, 2)
  val ThreeMonths = Tenor(Month, 3)
  val SixMonths   = Tenor(Month, 6)
  val NineMonths  = Tenor(Month, 9)
  val TwelveMonths = Tenor(Month, 12)
  val OneYear     = Tenor(Year, 1)
}