package starling.reports.pivot

import starling.daterange.{Period, DateRange}

case class OptionalPeriodLabel(period: Option[Period]) {
  override def toString = period match {
    case None => ""
    case Some(dr) => dr.toString
  }
}

object OptionalPeriodLabel {
  val Null  = OptionalPeriodLabel(None)
}