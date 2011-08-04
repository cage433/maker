package starling.daterange

import starling.utils.ImplicitConversions._
import starling.utils.Pattern._

/**
 * How an index's fixing period is represented in the database
 */
case class StoredFixingPeriod(period: Either[DateRange, Tenor]) extends Ordered[StoredFixingPeriod] {
  lazy val (dateRange, tenor) = (period.left.toOption, period.right.toOption)
  def compare(that: StoredFixingPeriod) = period.compareEither(that.period)
  def toDateRange(day: Day) = period.fold(identity _, _.toDateRange(day))
  override def toString = period.fold(_.toString, _.toString)
}

object StoredFixingPeriod {
  def dateRange(dateRange: DateRange) = StoredFixingPeriod(Left(dateRange))
  def tenor(tenor: Tenor)             = StoredFixingPeriod(Right(tenor))

  val DateRange = Extractor.from[StoredFixingPeriod](_.dateRange)
  val Tenor     = Extractor.from[StoredFixingPeriod](_.tenor)

  val Parse = Extractor.from[String](input => input partialMatch {
    case starling.daterange.Tenor.Parse(value) => tenor(value)
    case starling.daterange.DateRange(value) => dateRange(value)
  })

  val Comparator = new Ordering[Any] {
    def compare(x: Any, y: Any) = toStoredFixingPeriod(x).compare(toStoredFixingPeriod(y))

    private def toStoredFixingPeriod(any: Any) = any match {
      case sfp: StoredFixingPeriod => sfp
      case other => parse(other)
    }
  }

  def parse(any: Any): StoredFixingPeriod = Parse.unapply(any.toString).getOrElse(throw new Exception("Could not parse: " + any))

}
