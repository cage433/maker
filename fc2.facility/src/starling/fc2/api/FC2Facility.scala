package starling.fc2.api

import starling.daterange.Day
import starling.rmi.PivotData
import starling.pivot.{PivotEdits, PivotFieldParams}
import starling.gui.api._
import starling.manager.Memoize

case class FC2InitialData(
  snapshots:Map[MarketDataSelection,List[SnapshotIDLabel]],
  observationDays:(Map[PricingGroup,Set[Day]], Map[String,Set[Day]]),
  pricingGroups:List[PricingGroupDefinition],
  environmentRuleLabels:Map[PricingGroup,List[EnvironmentRuleLabel]],
  excelDataSets:List[String],
  excelLatestMarketDataVersions:Map[String,Int],
  pricingGroupLatestMarketDataVersions:Map[PricingGroup,Int],
  curveTypes:List[CurveTypeLabel]
)
trait FC2Facility {
  def init():FC2InitialData

  def importData(marketDataSelection:MarketDataSelection, observationDay:Day):MarketDataVersion
  def snapshot(marketDataIdentifier:MarketDataIdentifier, snapshotType:SnapshotType):SnapshotIDLabel

  def latestMarketDataIdentifier(selection:MarketDataSelection):MarketDataIdentifier

  @Memoize def curvePivot(curveLabel: CurveLabel, pivotFieldParams:PivotFieldParams): PivotData
  @Memoize def readAllMarketData(marketDataIdentifier:MarketDataPageIdentifier, marketDataType:Option[MarketDataTypeLabel], edits:PivotEdits, pivotFieldParams:PivotFieldParams):PivotData
  def saveMarketData(marketDataIdentifier:MarketDataPageIdentifier, marketDataTypeLabel:Option[MarketDataTypeLabel], pivotEdits:PivotEdits):Int
}