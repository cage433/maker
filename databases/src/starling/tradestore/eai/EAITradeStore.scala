package starling.tradestore.eai

import starling.instrument.TradeableType
import starling.daterange.Timestamp
import starling.eai._
import starling.instrument.TradeAttributes
import starling.db.EAITradeSystem
import starling.pivot._
import starling.richdb.{RichInstrumentResultSetRow, RichDB}
import starling.tradestore.{TradeStore}
import collection.immutable.{List, Set}
import starling.utils.Broadcaster
import starling.dbx.QueryBuilder._
import starling.gui.api.{EAIDeskInfo, Desk}

object EAITradeStore {
  val desk_str = "Desk"
  val dealID_str = "Deal ID"
  val trader_str = "Trader"
  val tradedFor_str = "Traded For"
  val broker_str = "Broker"
  val clearing_str = "Clearing House"
}
case class EAITradeAttributes(strategyID: TreeID, bookID: TreeID, dealID: TreeID,
                              trader: String, tradedFor: String, broker: String, clearingHouse: String) extends TradeAttributes {
  require(strategyID != null)
  require(bookID != null)
  require(dealID != null)
  require(trader != null)
  require(tradedFor != null)
  require(broker != null)
  require(clearingHouse != null)
  lazy val desk = Desk.eaiDeskFromID(bookID.id).getOrElse(throw new Exception("Invalid book id: " + bookID.id))

  import EAITradeStore._
  def details = Map(
    "StrategyID" -> strategyID.id,
    dealID_str -> dealID.id,
    trader_str -> trader,
    tradedFor_str -> tradedFor,
    broker_str -> broker,
    clearing_str -> clearingHouse
  )

  override def createFieldValues = Map(
    Field(desk_str) -> desk.name,
    //Strategy is intentionally excluded as it needs to hold the path, not just the id, so it is added in #joiningTradeAttributeFieldValues
    Field(dealID_str) -> dealID,
    Field(trader_str) -> trader,
    Field(tradedFor_str) -> tradedFor,
    Field(broker_str) -> broker,
    Field(clearing_str) -> clearingHouse
  )
}


class EAITradeStore(db: RichDB, broadcaster:Broadcaster, eaiStrategyDB:EAIStrategyDB, desk:Desk) extends
    TradeStore(db, broadcaster, EAITradeSystem) {

  lazy val usedStrategyIDs = new scala.collection.mutable.HashSet[Int]()
  val tableName = "EAITrade_book_" + desk.deskInfo.get.asInstanceOf[EAIDeskInfo].book

  val tradeAttributesFactory = EAITradeAttributes

  populateUsedIds()

  def createTradeAttributes(row: RichInstrumentResultSetRow) = {
    import EAITradeStore._
    val bookID = desk.deskInfo.get.asInstanceOf[EAIDeskInfo].book
    val strategyID = row.getInt("StrategyID")
    val dealID = row.getInt(dealID_str.replaceAll(" ", ""))
    val trader = row.getStringOrNone(trader_str).getOrElse("")
    val tradedFor = row.getStringOrNone(tradedFor_str.replaceAll(" ", "")).getOrElse("")
    val broker = row.getStringOrNone(broker_str).getOrElse("")
    val clearer = row.getStringOrNone(clearing_str.replaceAll(" ", "")).getOrElse("")
    EAITradeAttributes(TreeID(strategyID), TreeID(bookID), TreeID(dealID), trader, tradedFor, broker, clearer)
  }

  override def joiningTradeAttributeFieldValues(tradeAttributes:TradeAttributes) = {
    Map(Field("Strategy") -> eaiStrategyDB.pathFor(tradeAttributes.asInstanceOf[EAITradeAttributes].strategyID))
  }

  override val tradeAttributeFieldDetails = {
    import EAITradeStore._
    List(
      FieldDetails(desk_str),
      new StrategyFieldDetails(new ExternalSortIndexPivotTreePathOrdering(eaiStrategyDB)),
      FieldDetails(dealID_str),
      FieldDetails(trader_str),
      FieldDetails(tradedFor_str),
      FieldDetails(broker_str),
      FieldDetails(clearing_str)
    )
  }

  private def populateUsedIds() {
    usedStrategyIDs.clear
    usedStrategyIDs ++= db.queryWithResult("select distinct strategyID from " + tableName, Map()) { rs=> rs.getInt("strategyID") }
  }
  override def tradesChanged() = {
    populateUsedIds
  }

  def pivotInitialState(tradeableTypes:Set[TradeableType[_]]) = {
    PivotFieldsState(
        List(Field("Trade Count"))
      )
  }

  def pivotDrillDownGroups() = {
    List(
      DrillDownInfo(PivotAxis( List(), List(), List(), false)),
      DrillDownInfo(PivotAxis( List(), List(Field("Instrument"), Field("Market")), List(), false)),
      instrumentFilteredDrillDown
    )

  }

// override def tradeAttributeColumns(tradeTableAlias:String):List[ColumnDefinition] =
//    List(
//      new TreeColumnDefinition("Strategy", "strategyID", tradeTableAlias, usedStrategyIDs, strategyTree),
//      new StringColumnDefinition("Book", "bookID", tradeTableAlias),
//      new StringColumnDefinition("Deal", "dealID", tradeTableAlias),
//      new StringColumnDefinition("Trader", "trader", tradeTableAlias),
//      new StringColumnDefinition("Traded For", "tradedFor", tradeTableAlias),
//      new StringColumnDefinition("Broker", "broker", tradeTableAlias),
//      new StringColumnDefinition("Timestamp", "timestamp", tradeTableAlias)
//      )
//
}

case class TreePath(id:TreeID, path:List[String]) extends Ordered[TreePath] {
  override def toString() = path.mkString("/")
  def compare(other:TreePath) = toString.compareTo(other.toString)
}