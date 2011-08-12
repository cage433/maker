package starling.titan

import starling.tradestore.TradeStore
import starling.instrument.TradeableType
import starling.pivot.{DrillDownInfo, PivotAxis, PivotFieldsState, Field}
import starling.richdb.{RichDB, RichInstrumentResultSetRow}
import starling.utils.Broadcaster
import starling.trade.TradeSystem

object TitanTradeStore {
  val quotaID_str = "Quota ID"
  val tradeID_str = "Trade ID"
}

class TitanTradeStore(db: RichDB, broadcaster:Broadcaster, tradeSystem:TradeSystem)
        extends TradeStore(db, broadcaster, tradeSystem, None) {
  val tableName = "TitanTrade"
  def createTradeAttributes(row:RichInstrumentResultSetRow) = {
    val quotaID = row.getString("quotaID")
    val titanTradeID = row.getString("titanTradeID")
    TitanTradeAttributes(quotaID, titanTradeID)
  }
  def pivotInitialState(tradeableTypes:Set[TradeableType[_]]) = {
    PivotFieldsState(List(Field("Trade Count")))
  }
  def pivotDrillDownGroups() = {
    List(
      DrillDownInfo(PivotAxis( List(), List(), List(), false)),
      DrillDownInfo(PivotAxis( List(), List(Field("Instrument"), Field("Commodity")), List(), false)),
      instrumentFilteredDrillDown
    )
  }
}