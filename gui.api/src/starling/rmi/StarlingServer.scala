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
  def tradeChanges(tradeSelection:TradeSelection,from:Timestamp,to:Timestamp, expiryDay:Day, pivotFieldParams:PivotFieldParams):PivotData
  def tradeReconciliation(tradeSelection:TradeSelection, from:TradeTimestamp, to:TradeTimestamp, intradayTimestamp: Timestamp, pivotFieldParams:PivotFieldParams):PivotData

  def readTradeVersions(tradeID:TradeIDLabel):(STable,List[FieldDetailsGroupLabel],List[CostsLabel])
  def tradeIDFor(desk:Desk, text:String):TradeIDLabel
  def tradePivot(tradeSelection:TradeSelectionWithTimestamp,expiryDay:Day,pivotFieldParams:PivotFieldParams):PivotData
  def bookClose(desk: Desk): Unit
  def importTitanTrades()
  def tradeImportText(tradeSelection:TradeSelection):(String,String)

  def version:Version
  def deskCloses: Map[Desk, Map[Day, List[TradeTimestamp]]]
  def latestTradeTimestamp(desk:Desk):TradeTimestamp
  def intradayLatest: Map[String, (User, Timestamp)]
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
  def gitLog(pivotFieldParams:PivotFieldParams, numCommits:Int):PivotData

  def rabbitEvents(pivotFieldParams:PivotFieldParams, latestID:Long):PivotData
  def latestRabbitEvent:Long
}