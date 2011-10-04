package starling.fc2.api

import starling.daterange.Day
import starling.rmi.PivotData
import starling.pivot.{PivotEdits, PivotFieldParams}
import starling.gui.api._

trait FC2Service {
  def snapshots():Map[MarketDataSelection,List[SnapshotIDLabel]]
  def observationDays():(Map[PricingGroup,Set[Day]], Map[String,Set[Day]])
  def pricingGroups():List[PricingGroup]
  def environmentRules():Map[PricingGroup,List[EnvironmentRuleLabel]]
  def curveTypes():List[CurveTypeLabel]
  def excelDataSets():List[String]
  def snapshot(marketDataSelection:MarketDataSelection, observationDay:Day): Option[SnapshotIDLabel]
  def excelLatestMarketDataVersions:Map[String,Int]
  def pricingGroupLatestMarketDataVersions:Map[PricingGroup,Int]

  def curvePivot(curveLabel: CurveLabel, pivotFieldParams:PivotFieldParams): PivotData
  def readAllMarketData(marketDataIdentifier:MarketDataPageIdentifier, marketDataType:Option[MarketDataTypeLabel], edits:PivotEdits, pivotFieldParams:PivotFieldParams):PivotData
  def saveMarketData(marketDataIdentifier:MarketDataPageIdentifier, marketDataTypeLabel:Option[MarketDataTypeLabel], pivotEdits:PivotEdits):Boolean

  def marketDataTypeLabels(marketDataIdentifier:MarketDataPageIdentifier):List[MarketDataTypeLabel]

  def latestMarketDataIdentifier(selection:MarketDataSelection):MarketDataIdentifier
}