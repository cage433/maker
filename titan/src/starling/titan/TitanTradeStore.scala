package starling.titan

import starling.richdb.{RichDB, RichInstrumentResultSetRow}
import starling.manager.Broadcaster
import starling.gui.api.{Desk, TradesUpdated}
import starling.instrument.{Trade, TradeableType, TradeSystem}
import EDMConversions._
import collection.immutable.Map
import starling.instrument.physical.{PhysicalMetalAssignmentOrUnassignedSalesQuota, PhysicalMetalForward}
import starling.marketdata._
import starling.pivot._
import starling.gui.api.Desk
import starling.tradestore.{RichTradeStore, TradeableFields, TradeRow, TradeStore}
import starling.tradeimport.ClosedDesks

object TitanTradeStore {
  val quotaID_str = "Quota ID"
  val quotaQuantity_str = "Quota Quantity"
  val assignmentID_str = "Assignment ID"
  val titanTradeID_str = "Titan Trade ID"
  val inventoryID_str = "Inventory ID"
  val groupCompany_str = "Group Company"
  val comment_str = "Comment"
  val submitted_str = "Submitted"
  val shape_str = "Shape"
  val contractFinalised_str = "Contract Finalised"
  val tolerancePlus_str = "Tolerance Plus"
  val toleranceMinus_str = "Tolerance Minus"
  val eventID_str = "Event ID" // this is the unique id of the event that resulted in this version of the trade

  val labels = List(quotaID_str, assignmentID_str, titanTradeID_str,
                    inventoryID_str, groupCompany_str, comment_str,
                    submitted_str, shape_str, contractFinalised_str,
                    tolerancePlus_str, toleranceMinus_str, eventID_str)

  val qtyLabels = List(quotaQuantity_str)
}

class TitanTradeStore(db: RichDB, broadcaster:Broadcaster, tradeSystem:TradeSystem, refDataLookup : ReferenceDataLookup, closedDesks: ClosedDesks)
        extends RichTradeStore(db, tradeSystem, closedDesks) {
  val tableName = "TitanTrade"
  def deskOption = Some(Desk.Titan)
  def createTradeAttributes(row:RichInstrumentResultSetRow) = {
    val quotaID = row.getString("quotaID")
    val quotaQuantity = row.getQuantity("quotaQuantity")
    val titanTradeID = row.getString("titanTradeID")
    val inventoryID = row.getString("inventoryID")
    val groupCompany = row.getString("groupCompany")
    val comment = Option(row.getString("Comment")).getOrElse("")
    val submitted = row.getDay("Submitted")
    val shape = row.getString("Shape")
    val contractFinalised = row.getString("ContractFinalised")
    val tolerancePlus = row.getPercentage("TolerancePlus")
    val toleranceMinus = row.getPercentage("ToleranceMinus")
    val eventID = row.getString("EventID")

    TitanTradeAttributes(quotaID, quotaQuantity, titanTradeID, Option(inventoryID), groupCompany, comment, submitted, shape, contractFinalised, tolerancePlus, toleranceMinus, eventID)
  }

  def pivotInitialState(tradeableTypes:Set[TradeableType[_]]) = {
    val pfs = PivotFieldsState(List(Field("Trade Count")))
    DefaultPivotState(pfs)
  }

  def pivotDrillDownGroups() = {
    List(
      DrillDownInfo(PivotAxis( List(), List(), List(), false)),
      DrillDownInfo(PivotAxis( List(), List(Field("Instrument"), Field("Commodity")), List(), false)),
      instrumentFilteredDrillDown
    )
  }


  override val tradeAttributeFieldDetails =
    TitanTradeStore.labels.map{ label => FieldDetails(label)} ++ TitanTradeStore.qtyLabels.map(lbl => new QuantityLabelFieldDetails(lbl))

  override def tradeableFieldDetails:List[FieldDetails] = (TradeableFields.fieldDetails :::
    FieldDetails.coded("Benchmark Country Code", refDataLookup.countries.values) ::
    FieldDetails.coded("Contract Location Code", refDataLookup.contractLocations.values) ::
    FieldDetails.coded("Grade Code", refDataLookup.grades.values) ::
    FieldDetails.coded("Contract Inco Term Code", refDataLookup.incoterms.values) ::
    FieldDetails.coded("Benchmark Inco Term Code", refDataLookup.incoterms.values) :: Nil).map(f => f.field -> f).toMap.values.toList

  override def tradesChanged() = {
    broadcaster.broadcast(TradesUpdated(Desk.Titan, latestKnownTimestamp.get))
  }

  def allStarlingTrades() = {
    readLatestVersionOfAllTrades().values.map(_.trade)
  }

  def getAllForwards() : Map[String, Either[String, PhysicalMetalForward]] = {
    val trades = allStarlingTrades().collect{t : Trade => t.titanTradeID match { case Some(id) => t }}
    trades.groupBy{trade : Trade => trade.titanTradeID.get}.map{
      case (titanTradeID, trades) => titanTradeID -> PhysicalMetalForwardBuilder(trades.toList)
    }.toMap
  }

  def getTradesForTitanTradeID(titanTradeID : String) : List[Trade] = {
    readLatestVersionOfAllTrades().flatMap{
      case (_, TradeRow(_, trade)) if trade.titanTradeID == Some(titanTradeID) => Some(trade)
      case _ => None
    }.toList
  }

  def getForward(titanTradeID : String) : Either[String, PhysicalMetalForward] = {
    PhysicalMetalForwardBuilder(getTradesForTitanTradeID(titanTradeID))
  }
}

