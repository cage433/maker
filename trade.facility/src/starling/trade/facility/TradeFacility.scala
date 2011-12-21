package starling.trade.facility

import starling.daterange.{Day, Timestamp}
import starling.pivot.PivotFieldParams
import starling.rmi.PivotData
import starling.utils.{STable, CaseInsensitive}
import starling.gui.api._
import starling.manager.Memoize
import starling.auth.User

case class TradeInitialData(
  desks:List[Desk],
  groupToDesksMap:Map[CaseInsensitive, Set[Desk]],
  deskCloses: Map[Desk, Map[Day, List[TradeTimestamp]]],
  intradayLatest: Map[String, (User, Timestamp)]
)

trait TradeFacility {
  def init():TradeInitialData
  @Memoize def tradeChanges(tradeSelection:TradeSelection,from:Timestamp,to:Timestamp, expiryDay:Day, pivotFieldParams:PivotFieldParams):PivotData
  @Memoize def tradeReconciliation(tradeSelection:TradeSelection, from:TradeTimestamp, to:TradeTimestamp, intradayTimestamp: Timestamp, pivotFieldParams:PivotFieldParams):PivotData
  def readTradeVersions(tradeID:TradeIDLabel):(STable,List[FieldDetailsGroupLabel],List[CostsLabel])
  @Memoize def tradeIDFor(desk:Desk, text:String):TradeIDLabel
  @Memoize def tradePivot(tradeSelection:TradeSelectionWithTimestamp,expiryDay:Day,pivotFieldParams:PivotFieldParams):PivotData
  def bookClose(desk: Desk): Unit
  def importTitanTrades()
  def tradeImportText(tradeSelection:TradeSelection):(String,String)
  def latestTradeTimestamp(desk:Desk):TradeTimestamp

}