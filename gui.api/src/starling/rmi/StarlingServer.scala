package starling.rmi

import starling.gui.UserSettings
import starling.gui.api._
import starling.services.{ForwardCurveCommitResponse, ForwardCurveInfo}
import starling.pivot._
import starling.auth.User
import starling.calendar.BusinessCalendar
import starling.eai.{Book, Traders}
import starling.utils.{CaseInsensitive, STable}
import starling.daterange.{ObservationPoint, Day, Timestamp}

/**
 * This is the remote interface to the starling server.
 */
trait StarlingServer {
  def name:String
  def desks:List[Desk]
  def groupToDesksMap:Map[CaseInsensitive, Set[Desk]]
  def reportOptionsAvailable:ReportOptionsAvailable
  def reportPivot(reportParameters:ReportParameters, pivotFieldParams:PivotFieldParams):PivotData
  def reportPivot(reportParameters:ReportParameters, layoutName:String):PivotData
  def diffReportPivot(tradeSelection:TradeSelection, curveIdentifierDm1:CurveIdentifierLabel, curveIdentifierD:CurveIdentifierLabel, reportOptions:ReportOptions, expiryDay:Day,fromTimestamp:TradeTimestamp, toTimestamp:TradeTimestamp, pivotFieldParams:PivotFieldParams):PivotData
  def tradeChanges(tradeSelection:TradeSelection,from:Timestamp,to:Timestamp, expiryDay:Day, pivotFieldParams:PivotFieldParams):PivotData
  def tradeReconciliation(tradeSelection:TradeSelection, from:TradeTimestamp, to:TradeTimestamp, intradayTimestamp: Timestamp, pivotFieldParams:PivotFieldParams):PivotData
  def pnlReconciliation(tradeSelection: TradeSelectionWithTimestamp, curveIdentifier: CurveIdentifierLabel, expiryDay:Day, pivotFieldParams: PivotFieldParams): PivotData
  def reportErrors(reportParameters:ReportParameters):ReportErrors
  def snapshots():Map[MarketDataSelection,List[SnapshotIDLabel]]
  def observationDays():(Map[PricingGroup,Set[Day]], Map[String,Set[Day]])
  def pricingGroups():List[PricingGroup]
  def environmentRules():Map[PricingGroup,List[EnvironmentRuleLabel]]
  def curveTypes():List[CurveTypeLabel]
  def excelDataSets():List[String]
  def snapshot(marketDataSelection:MarketDataSelection, observationDay:Day):SnapshotIDLabel
  def excelLatestMarketDataVersions:Map[String,Int]
  def pricingGroupLatestMarketDataVersions:Map[PricingGroup,Int]
  def readSettings:UserSettings
  def saveSettings(settings:UserSettings)
  def readTradeVersions(tradeID:TradeIDLabel):(STable,List[FieldDetailsGroupLabel],List[CostsLabel])
  def tradeIDFor(desk:Desk, text:String):TradeIDLabel
  def tradeValuation(tradeID:TradeIDLabel, curveIdentifier:CurveIdentifierLabel, timestamp:Timestamp):TradeValuation
  def tradePivot(tradeSelection:TradeSelectionWithTimestamp,expiryDay:Day,pivotFieldParams:PivotFieldParams):PivotData
  def importAllTrades(desk:Desk):Boolean
  def bookClose(desk: Desk): Unit
  def tradeImportText(tradeSelection:TradeSelection):(String,String)

  def curvePivot(curveLabel: CurveLabel, pivotFieldParams:PivotFieldParams): PivotData
  def readAllMarketData(marketDataIdentifier:MarketDataPageIdentifier, marketDataType:Option[MarketDataTypeLabel], pivotFieldParams:PivotFieldParams):PivotData
  def saveMarketData(marketDataIdentifier:MarketDataPageIdentifier, marketDataTypeLabel:Option[MarketDataTypeLabel], pivotEdits:Set[PivotEdit]):Boolean

  def version:Version
  def deskCloses: Map[Desk, Map[Day, List[TradeTimestamp]]]
  def intradayLatest: Map[String, (User, Timestamp)]
  def clearCache:Unit
  def marketDataTypeLabels(marketDataIdentifier:MarketDataPageIdentifier):List[MarketDataTypeLabel]
  def extraLayouts:List[PivotLayout]
  def saveLayout(pivotLayout:PivotLayout):Unit
  def deleteLayout(layoutName:String):Unit
  def userReports:List[UserReport]
  def createReportParameters(userReportData:UserReportData, observationDay:Day):ReportParameters
  def createUserReport(reportParameters:ReportParameters):UserReportData
  def referenceDataTables():List[ReferenceDataLabel]
  def referencePivot(table:ReferenceDataLabel, pivotFieldParams:PivotFieldParams):PivotData
  def ukBusinessCalendar:BusinessCalendar

  // horrible hack, we need to figure out how to do this properly
  def permissionToDoAdminLikeThings: Boolean
  def whoAmI:User
  def traders: Map[User,(Book, Desk)]
  def orgPivot(pivotFieldParams:PivotFieldParams):PivotData
  def allUserNames:List[String]
  def isStarlingDeveloper:Boolean

  def logPageView(pageLogInfo:PageLogInfo):Unit
  def userStatsPivot(pivotFieldParams:PivotFieldParams):PivotData

  def storeSystemInfo(info:OSInfo)

  def saveBookmark(bookmark:BookmarkLabel)
  def deleteBookmark(name:String)

  def bookmarks:List[BookmarkLabel]

  def createTradeSelectionBookmarkData(tpp:TradePageParameters):TradeSelectionBookmarkData
  def createTradePageParameters(tradeSelectionBookmarkData:TradeSelectionBookmarkData, observationDay:Day):TradePageParameters
}