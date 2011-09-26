package starling.trade

import starling.daterange.{Day, Timestamp}
import starling.pivot.PivotFieldParams
import starling.rmi.PivotData
import starling.utils.{STable, CaseInsensitive}
import starling.gui.api._
import starling.manager.DoNotCache
import starling.auth.User

trait TradeService {
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