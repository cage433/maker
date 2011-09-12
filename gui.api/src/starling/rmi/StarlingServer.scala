package starling.rmi

import starling.gui.api._
import starling.services.{ForwardCurveCommitResponse, ForwardCurveInfo}
import starling.pivot._
import starling.auth.User
import starling.calendar.BusinessCalendar
import starling.eai.{Traders}
import starling.utils.{CaseInsensitive, STable}
import starling.daterange.{ObservationPoint, Day, Timestamp}
import starling.browser.service.{PageLogInfo, BookmarkLabel, UserSettingsLabel, Version}

/**
 * This is the remote interface to the starling server.
 */
trait StarlingServer {
  def name:String
  def desks:List[Desk]
  def groupToDesksMap:Map[CaseInsensitive, Set[Desk]]
  def reportOptionsAvailable:ReportOptionsAvailable
  def reportPivot(reportParameters:ReportParameters, pivotFieldParams:PivotFieldParams):PivotData
  def diffReportPivot(tradeSelection:TradeSelection, curveIdentifierDm1:CurveIdentifierLabel, curveIdentifierD:CurveIdentifierLabel, reportOptions:ReportOptions, expiryDay:Day,fromTimestamp:TradeTimestamp, toTimestamp:TradeTimestamp, pivotFieldParams:PivotFieldParams):PivotData
  def tradeChanges(tradeSelection:TradeSelection,from:Timestamp,to:Timestamp, expiryDay:Day, pivotFieldParams:PivotFieldParams):PivotData
  def tradeReconciliation(tradeSelection:TradeSelection, from:TradeTimestamp, to:TradeTimestamp, intradayTimestamp: Timestamp, pivotFieldParams:PivotFieldParams):PivotData
  def pnlReconciliation(tradeSelection: TradeSelectionWithTimestamp, curveIdentifier: CurveIdentifierLabel, expiryDay:Day, pivotFieldParams: PivotFieldParams): PivotData
  def reportErrors(reportParameters:ReportParameters):ReportErrors

  def readTradeVersions(tradeID:TradeIDLabel):(STable,List[FieldDetailsGroupLabel],List[CostsLabel])
  def tradeIDFor(desk:Desk, text:String):TradeIDLabel
  def tradeValuation(tradeID:TradeIDLabel, curveIdentifier:CurveIdentifierLabel, timestamp:Timestamp):TradeValuationAndDetails
  def tradePivot(tradeSelection:TradeSelectionWithTimestamp,expiryDay:Day,pivotFieldParams:PivotFieldParams):PivotData
  def bookClose(desk: Desk): Unit
  def importTitanTrades()
  def tradeImportText(tradeSelection:TradeSelection):(String,String)

  def version:Version
  def deskCloses: Map[Desk, Map[Day, List[TradeTimestamp]]]
  def latestTradeTimestamp(desk:Desk):TradeTimestamp
  def intradayLatest: Map[String, (User, Timestamp)]
  def clearCache:Unit
  def createReportParameters(userReportData:UserReportData, observationDay:Day):ReportParameters
  def createUserReport(reportParameters:ReportParameters):UserReportData
  def referenceDataTables():List[ReferenceDataLabel]
  def referencePivot(table:ReferenceDataLabel, pivotFieldParams:PivotFieldParams):PivotData
  def ukBusinessCalendar:BusinessCalendar
  def whoAmI:User
  def traders: Map[User, List[Desk]]
  def orgPivot(pivotFieldParams:PivotFieldParams):PivotData
  def allUserNames:List[String]
  def isStarlingDeveloper:Boolean

  def userStatsPivot(pivotFieldParams:PivotFieldParams):PivotData

  def storeSystemInfo(info:OSInfo)

  def deleteUserReport(reportName:String)
  def saveUserReport(reportName:String, data:UserReportData, showParameters:Boolean)

}