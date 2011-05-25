package starling.daterange

import starling.utils.Pattern._


case class Tenor(tenorName: String, value: Int) extends Ordered[Tenor] {
  lazy val tenorType = TenorType.typesByShortName(tenorName)

  def compare(that: Tenor) = indexOf(tenorName).compare(indexOf(that.tenorName)) match {
    case 0 => value.compare(that.value)
    case other => other
  }

  override def toString = value + tenorName
  private def indexOf(tenor: String) = TenorType.ALL_IN_ORDER.indexOf(tenor)
}

object Tenor {
  def apply(tenorType: TenorType, value: Int): Tenor = Tenor(tenorType.shortName, value)

  val ON          = Tenor(Day, 0)   // Overnight
  val SN          = Tenor(Day, 0)   // Spot Next
  val OneDay      = Tenor(Day, 1)
  val OneWeek     = Tenor(Week, 1)
  val TwoWeeks    = Tenor(Week, 2)
  val OneMonth    = Tenor(Month, 1)
  val TwoMonths   = Tenor(Month, 2)
  val ThreeMonths = Tenor(Month, 3)
  val SixMonths   = Tenor(Month, 6)
  val NineMonths  = Tenor(Month, 9)
  val OneYear     = Tenor(Year, 1)
}

case class StoredFixingPeriod(period: Either[DateRange, Tenor]) extends Ordered[StoredFixingPeriod] {
  lazy val (dateRange, tenor) = (period.left.toOption, period.right.toOption)
  def compare(that: StoredFixingPeriod) = (period, that.period) match {
    case (Left(leftDR), Left(rightDR)) => leftDR.compare(rightDR)
    case (Right(leftOffset), Right(rightOffset)) => leftOffset.compare(rightOffset)
    case (Left(_), Right(_)) => 1
    case (Right(_), Left(_)) => -1
  }

  override def toString = period.fold(_.toString, tenor => if (tenor.value == 0) "CASH" else tenor.toString)
}

object StoredFixingPeriod {
  def dateRange(dateRange: DateRange) = StoredFixingPeriod(Left(dateRange))
  def tenor(tenor: Tenor)             = StoredFixingPeriod(Right(tenor))

  val DateRange = Extractor.from[StoredFixingPeriod](_.dateRange)
  val Tenor     = Extractor.from[StoredFixingPeriod](_.tenor)
}
