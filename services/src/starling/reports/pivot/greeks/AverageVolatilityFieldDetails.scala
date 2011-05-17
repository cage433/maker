package starling.reports.pivot.greeks

import starling.utils.ImplicitConversions._
import starling.quantity.Percentage
import starling.pivot._

class AverageVolatilityFieldDetails(name:String) extends FieldDetails(name) {
  override def isDataField = true
  override def value(a: Any) = {
    val vols = a.asInstanceOf[RiskVols].vols
    if (vols.isEmpty) {
      Set()
    } else {
      val values = vols.values
      val stdDeviations = values.flatMap { v => v.measure match { case Left(d) => List(d); case _ => List() }}
      val percentages = values.flatMap { v => v.measure match { case Right(p) => List(p); case _ => List() }}
      val averagePercentage = if (percentages.nonEmpty) Some(Percentage(percentages.map(_.value).sum / percentages.size)) else None
      val averageStdDeviation = if (stdDeviations.nonEmpty) Some(stdDeviations.sum / stdDeviations.size) else None
      (averagePercentage, averageStdDeviation) match {
        case (None, None) => Set()
        case (None, Some(s)) => PivotQuantity(s)
        case (Some(p), None) => p
        case (Some(p), Some(s)) => List(p,s)
      }
    }
  }
  override def formatter = AverageVolatilityPivotFormatter
  override def combineGroup(groupA: Any, groupB: Any) = groupA.asInstanceOf[RiskVols] ++ groupB.asInstanceOf[RiskVols]
  override def combine(group: Any, value: Any) = group.asInstanceOf[RiskVols] ++ value.asInstanceOf[RiskVols]
  override def nullGroup() = RiskVols.Null
  override def nullValue() = RiskVols.Null
}
