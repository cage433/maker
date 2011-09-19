package starling.rmi

import starling.gui.api._
import starling.services.{ForwardCurveCommitResponse, ForwardCurveInfo}
import starling.pivot._
import starling.auth.User
import starling.calendar.BusinessCalendar
import starling.eai.{Traders}
import starling.daterange.{ObservationPoint, Day, Timestamp}
import starling.browser.service.{PageLogInfo, BookmarkLabel, UserSettingsLabel, Version}
import starling.utils.{CaseInsensitive, STable}
import starling.manager.DoNotCache

/**
 * This is the remote interface to the starling server.
 */
trait StarlingServer {
  def name:String
  def version:Version

  def referenceDataTables():List[ReferenceDataLabel]
  def referencePivot(table:ReferenceDataLabel, pivotFieldParams:PivotFieldParams):PivotData
  def ukBusinessCalendar:BusinessCalendar

  def whoAmI:User
  @DoNotCache def orgPivot(pivotFieldParams:PivotFieldParams):PivotData
  @DoNotCache def allUserNames:List[String]
  def isStarlingDeveloper:Boolean
  @DoNotCache def userStatsPivot(pivotFieldParams:PivotFieldParams):PivotData
  @DoNotCache def storeSystemInfo(info:OSInfo)
  def gitLog(pivotFieldParams:PivotFieldParams, numCommits:Int):PivotData

  def desks:List[Desk]
  def groupToDesksMap:Map[CaseInsensitive, Set[Desk]]
  def tradeChanges(tradeSelection:TradeSelection,from:Timestamp,to:Timestamp, expiryDay:Day, pivotFieldParams:PivotFieldParams):PivotData
  def tradeReconciliation(tradeSelection:TradeSelection, from:TradeTimestamp, to:TradeTimestamp, intradayTimestamp: Timestamp, pivotFieldParams:PivotFieldParams):PivotData

  def readTradeVersions(tradeID:TradeIDLabel):(STable,List[FieldDetailsGroupLabel],List[CostsLabel])
  def tradeIDFor(desk:Desk, text:String):TradeIDLabel
  def tradePivot(tradeSelection:TradeSelectionWithTimestamp,expiryDay:Day,pivotFieldParams:PivotFieldParams):PivotData
  @DoNotCache def bookClose(desk: Desk): Unit
  @DoNotCache def importTitanTrades()
  @DoNotCache def tradeImportText(tradeSelection:TradeSelection):(String,String)
  @DoNotCache def deskCloses: Map[Desk, Map[Day, List[TradeTimestamp]]]
  @DoNotCache def latestTradeTimestamp(desk:Desk):TradeTimestamp
  @DoNotCache def intradayLatest: Map[String, (User, Timestamp)]
  def traders: Map[User, List[Desk]]
}