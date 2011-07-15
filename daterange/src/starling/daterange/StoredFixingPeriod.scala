package starling.daterange

import starling.utils.ImplicitConversions._
import starling.utils.Pattern._

/**
 * How an index's fixing period is represented in the database
 */
case class StoredFixingPeriod(period: Either[DateRange, Tenor]) extends Ordered[StoredFixingPeriod] {
  lazy val (dateRange, tenor) = (period.left.toOption, period.right.toOption)
  def compare(that: StoredFixingPeriod) = (period, that.period) match {
    case (Left(leftDR), Left(rightDR)) => leftDR.compare(rightDR)
    case (Right(leftOffset), Right(rightOffset)) => leftOffset.compare(rightOffset)
    case (Left(_), Right(_)) => 1
    case (Right(_), Left(_)) => -1
  }

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

  def parse(any: Any): StoredFixingPeriod = Parse.unapply(any.toString).getOrElse(throw new Exception("Could not parse: " + any))
}
