package starling.fc2.api

import starling.daterange.Day
import starling.rmi.PivotData
import starling.pivot.{PivotEdits, PivotFieldParams}
import starling.gui.api._
import starling.manager.DoNotCache

case class FC2InitialData(
  snapshots:Map[MarketDataSelection,List[SnapshotIDLabel]],
  observationDays:(Map[PricingGroup,Set[Day]], Map[String,Set[Day]]),
  pricingGroups:List[PricingGroup],
  environmentRuleLabels:Map[PricingGroup,List[EnvironmentRuleLabel]],
  excelDataSets:List[String],
  excelLatestMarketDataVersions:Map[String,Int],
  pricingGroupLatestMarketDataVersions:Map[PricingGroup,Int],
  curveTypes:List[CurveTypeLabel]
)
trait FC2Facility {
  @DoNotCache def init():FC2InitialData

  @DoNotCache def importData(marketDataSelection:MarketDataSelection, observationDay:Day):MarketDataVersion
  @DoNotCache def snapshot(marketDataIdentifier:MarketDataIdentifier, snapshotType:SnapshotType):SnapshotIDLabel

  @DoNotCache def latestMarketDataIdentifier(selection:MarketDataSelection):MarketDataIdentifier

  def curvePivot(curveLabel: CurveLabel, pivotFieldParams:PivotFieldParams): PivotData
  def readAllMarketData(marketDataIdentifier:MarketDataPageIdentifier, marketDataType:Option[MarketDataTypeLabel], edits:PivotEdits, pivotFieldParams:PivotFieldParams):PivotData
  @DoNotCache def saveMarketData(marketDataIdentifier:MarketDataPageIdentifier, marketDataTypeLabel:Option[MarketDataTypeLabel], pivotEdits:PivotEdits):Int

  def marketDataTypeLabels(marketDataIdentifier:MarketDataPageIdentifier):List[MarketDataTypeLabel]

}