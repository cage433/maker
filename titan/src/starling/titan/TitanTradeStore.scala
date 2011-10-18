package starling.titan

import starling.richdb.{RichDB, RichInstrumentResultSetRow}
import starling.utils.Broadcaster
import starling.gui.api.{Desk, TradesUpdated}
import starling.tradestore.{TradeRow, TradeStore}
import starling.instrument.{Trade, TradeableType, TradeSystem}
import EDMConversions._
import collection.immutable.Map
import starling.instrument.physical.{PhysicalMetalAssignmentOrUnassignedSalesQuota, PhysicalMetalForward}
import starling.marketdata._
import starling.pivot._

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

class TitanTradeStore(db: RichDB, broadcaster:Broadcaster, tradeSystem:TradeSystem, refDataLookup : ReferenceDataLookup)
        extends TradeStore(db, broadcaster, tradeSystem) {
  val tableName = "TitanTrade"
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
    TitanTradeStore.labels.map{ label => FieldDetails(label)} ++ TitanTradeStore.qtyLabels.map(lbl => new QuantityLabelFieldDetails(lbl))

  override def tradesChanged() = {
   broadcaster.broadcast(TradesUpdated(Desk.Titan, cachedLatestTimestamp.get))
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
      case (_, TradeRow(_, _, trade)) if trade.titanTradeID == titanTradeID => Some(trade)
      case _ => None
    }.toList
  }

  def getForward(titanTradeID : String) : Either[String, PhysicalMetalForward] = {
    PhysicalMetalForwardBuilder(getTradesForTitanTradeID(titanTradeID))
  }

  override protected def addExtraInstrumentFields(map: Map[Field, Any]) = {
    import PhysicalMetalAssignmentOrUnassignedSalesQuota._
    val extraFields = map.flatMap {
      case (field, value) =>
        (field.name, value) match {
          case ("Benchmark Country Code", code: String) => Some(Field("Benchmark Country") -> refDataLookup.countryFor(NeptuneCountryCode(code)).name)
          case ("Contract Location Code", code: String) => Some(Field("Contract Location") -> refDataLookup.contractLocationFor(ContractualLocationCode(code)).name)
          case ("Grade Code", code: String) => Some(Field("Grade") -> refDataLookup.gradeFor(GradeCode(code)).name)
          case ("Contract Inco Term Code", code: String) => Some(Field("Contract Inco Term") -> refDataLookup.incotermFor(IncotermCode(code)).name)
          case ("Benchmark Inco Term Code", code: String) => Some(Field("Benchmark Inco Term") -> refDataLookup.incotermFor(IncotermCode(code)).name)
          case _ => None
        }
    }
    map ++ extraFields
  }

  override protected def addExtraInstrumentFieldDetails(list: List[FieldDetails]) = {
    val map = Map(
      "Benchmark Country Code" -> FieldDetails("Benchmark Country"),
      "Contract Location Code" -> FieldDetails("Contract Location"),
      "Benchmark Inco Term Code" -> FieldDetails("Benchmark Inco Term"),
      "Contract Inco Term Code" -> FieldDetails("Contract Inco Term"),
      "Grade Code" -> FieldDetails("Grade")
     )
    val extraDetails = list.flatMap{fd => map.get(fd.field.name)}
    list ::: extraDetails

  }
}

