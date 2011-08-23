package starling.titan

import starling.tradestore.TradeStore
import starling.instrument.TradeableType
import starling.pivot.{DrillDownInfo, PivotAxis, PivotFieldsState, Field}
import starling.richdb.{RichDB, RichInstrumentResultSetRow}
import starling.utils.Broadcaster
import starling.trade.TradeSystem
import starling.pivot.FieldDetails

object TitanTradeStore {
  val quotaID_str = "Quota ID"
  val tradeID_str = "Trade ID"
  val comment_str = "Comment"
  val submitted_str = "Submitted"
  val shape_str = "Shape"
  val grade_str = "Grade"
  val deliveryLocation_str = "Delivery Location"
  val destinationLocation_str = "Destination Location"
  val labels = List(quotaID_str, tradeID_str, comment_str, submitted_str, shape_str, grade_str, deliveryLocation_str, destinationLocation_str)
}

class TitanTradeStore(db: RichDB, broadcaster:Broadcaster, tradeSystem:TradeSystem)
        extends TradeStore(db, broadcaster, tradeSystem, None) {
  val tableName = "TitanTrade"
  def createTradeAttributes(row:RichInstrumentResultSetRow) = {
    val quotaID = row.getString("quotaID")
    val titanTradeID = row.getString("titanTradeID")
    val comment = row.getString("Comment")
    val submitted = row.getDay("Submitted")
    val shape = row.getString("Shape")
    val grade = row.getString("Grade")
    val deliveryLocation = row.getString("DeliveryLocation")
    val destinationLocation = row.getString("DestinationLocation")
      
    TitanTradeAttributes(quotaID, titanTradeID, comment, submitted, shape, grade, deliveryLocation, destinationLocation)
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

  override val tradeAttributeFieldDetails = {
    TitanTradeStore.labels.map{ label => FieldDetails(label)}
  }
}
