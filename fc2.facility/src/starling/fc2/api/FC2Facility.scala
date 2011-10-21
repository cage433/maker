package starling.fc2.api

import starling.daterange.Day
import starling.rmi.PivotData
import starling.pivot.{PivotEdits, PivotFieldParams}
import starling.gui.api._
import starling.manager.DoNotCache

trait FC2Facility {
  @DoNotCache def snapshots():Map[MarketDataSelection,List[SnapshotIDLabel]]
  @DoNotCache def observationDays():(Map[PricingGroup,Set[Day]], Map[String,Set[Day]])
  def pricingGroups():List[PricingGroup]
  def environmentRuleLabels():Map[PricingGroup,List[EnvironmentRuleLabel]]
  def curveTypes():List[CurveTypeLabel]
  @DoNotCache def excelDataSets():List[String]
  @DoNotCache def excelLatestMarketDataVersions:Map[String,Int]
  @DoNotCache def pricingGroupLatestMarketDataVersions:Map[PricingGroup,Int]

  @DoNotCache def importData(marketDataSelection:MarketDataSelection, observationDay:Day):MarketDataVersion
  @DoNotCache def snapshot(marketDataIdentifier:MarketDataIdentifier, snapshotType:SnapshotType):SnapshotIDLabel

  @DoNotCache def latestMarketDataIdentifier(selection:MarketDataSelection):MarketDataIdentifier

  def curvePivot(curveLabel: CurveLabel, pivotFieldParams:PivotFieldParams): PivotData
  def readAllMarketData(marketDataIdentifier:MarketDataPageIdentifier, marketDataType:Option[MarketDataTypeLabel], edits:PivotEdits, pivotFieldParams:PivotFieldParams):PivotData
  @DoNotCache def saveMarketData(marketDataIdentifier:MarketDataPageIdentifier, marketDataTypeLabel:Option[MarketDataTypeLabel], pivotEdits:PivotEdits):Int

  def marketDataTypeLabels(marketDataIdentifier:MarketDataPageIdentifier):List[MarketDataTypeLabel]

}