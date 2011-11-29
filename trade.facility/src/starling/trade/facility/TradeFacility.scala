package starling.trade.facility

import starling.daterange.{Day, Timestamp}
import starling.pivot.PivotFieldParams
import starling.rmi.PivotData
import starling.utils.{STable, CaseInsensitive}
import starling.gui.api._
import starling.manager.DoNotCache
import starling.auth.User

case class TradeInitialData(
  desks:List[Desk],
  groupToDesksMap:Map[CaseInsensitive, Set[Desk]],
  deskCloses: Map[Desk, Map[Day, List[TradeTimestamp]]],
  intradayLatest: Map[String, (User, Timestamp)]
)

trait TradeFacility {
  @DoNotCache def init():TradeInitialData
  def tradeChanges(tradeSelection:TradeSelection,from:Timestamp,to:Timestamp, expiryDay:Day, pivotFieldParams:PivotFieldParams):PivotData
  def tradeReconciliation(tradeSelection:TradeSelection, from:TradeTimestamp, to:TradeTimestamp, intradayTimestamp: Timestamp, pivotFieldParams:PivotFieldParams):PivotData
  def readTradeVersions(tradeID:TradeIDLabel):(STable,List[FieldDetailsGroupLabel],List[CostsLabel])
  def tradeIDFor(desk:Desk, text:String):TradeIDLabel
  def tradePivot(tradeSelection:TradeSelectionWithTimestamp,expiryDay:Day,pivotFieldParams:PivotFieldParams):PivotData
  @DoNotCache def bookClose(desk: Desk): Unit
  @DoNotCache def importTitanTrades()
  @DoNotCache def tradeImportText(tradeSelection:TradeSelection):(String,String)
  @DoNotCache def latestTradeTimestamp(desk:Desk):TradeTimestamp

}