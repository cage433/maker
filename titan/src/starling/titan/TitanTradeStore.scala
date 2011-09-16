package starling.titan

import starling.tradestore.TradeStore
import starling.instrument.TradeableType
import starling.pivot.{DrillDownInfo, PivotAxis, PivotFieldsState, Field}
import starling.richdb.{RichDB, RichInstrumentResultSetRow}
import starling.utils.Broadcaster
import starling.trade.TradeSystem
import starling.pivot.FieldDetails
import starling.gui.api.{Desk, TradesUpdated}

object TitanTradeStore {
  val quotaID_str = "Quota ID"
  val assignmentID_str = "Assignment ID"
  val titanTradeID_str = "Titan Trade ID"
  val inventoryID_str = "Inventory ID"
  val groupCompany_str = "Group Company"
  val comment_str = "Comment"
  val submitted_str = "Submitted"
  val shape_str = "Shape"
  val grade_str = "Grade"
  val deliveryLocation_str = "Delivery Location"
  val destinationLocation_str = "Destination Location"
  val contractFinalised_str = "Contract Finalised"
  val tolerancePlus_str = "Tolerance Plus"
  val toleranceMinus_str = "Tolerance Minus"
  val schedule_str = "Schedule"
  val expectedSales_str = "Expected Sales"

  val labels = List(quotaID_str, assignmentID_str, titanTradeID_str, inventoryID_str, groupCompany_str, comment_str, submitted_str, shape_str, grade_str, deliveryLocation_str, destinationLocation_str, contractFinalised_str, tolerancePlus_str, toleranceMinus_str, schedule_str, expectedSales_str)
}

class TitanTradeStore(db: RichDB, broadcaster:Broadcaster, tradeSystem:TradeSystem)
        extends TradeStore(db, broadcaster, tradeSystem, None) {
  val tableName = "TitanTrade"
  def createTradeAttributes(row:RichInstrumentResultSetRow) = {
    val quotaID = row.getString("quotaID")
    val titanTradeID = row.getString("titanTradeID")
    val assignmentID = row.getString("assignmentID")
    val inventoryID = row.getString("inventoryID")
    val groupCompany = row.getString("groupCompany")
    val comment = row.getString("Comment")
    val submitted = row.getDay("Submitted")
    val shape = row.getString("Shape")
    val grade = row.getString("Grade")
    val deliveryLocation = row.getString("DeliveryLocation")
    val destinationLocation = row.getString("DestinationLocation")
    val contractFinalised = row.getString("ContractFinalised")
    val tolerancePlus = row.getPercentage("TolerancePlus")
    val toleranceMinus = row.getPercentage("ToleranceMinus")
    val schedule = row.getDay("Schedule")
    val expectedSales = row.getDay("ExpectedSales")

    TitanTradeAttributes(assignmentID, quotaID, titanTradeID, inventoryID, groupCompany, comment, submitted, shape, grade, deliveryLocation, destinationLocation, contractFinalised, tolerancePlus, toleranceMinus, schedule, expectedSales)
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

  override val tradeAttributeFieldDetails =
    TitanTradeStore.labels.map{ label => FieldDetails(label)}

  override def tradesChanged() = {
   broadcaster.broadcast(TradesUpdated(Desk.Titan, cachedLatestTimestamp.get))
  }
}

