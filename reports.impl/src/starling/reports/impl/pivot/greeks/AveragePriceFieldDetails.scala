package starling.reports.impl.pivot.greeks

import starling.pivot._
import starling.reports.pivot.greeks.RiskPrices

class AveragePriceFieldDetails(name:String) extends FieldDetails(name) {
  override def isDataField = true
  override def value(a: Any) = a.asInstanceOf[RiskPrices].averagePrice.getOrElse(Set())
  override def formatter = EmptySetOrQuantityPivotFormatter
  override def combineGroup(groupA: Any, groupB: Any) = groupA.asInstanceOf[RiskPrices] ++ groupB.asInstanceOf[RiskPrices]
  override def combine(group: Any, value: Any) = group.asInstanceOf[RiskPrices] ++ value.asInstanceOf[RiskPrices]
  override def nullGroup() = RiskPrices.Null
  override def nullValue() = RiskPrices.Null
}
