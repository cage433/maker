package starling.tradestore.intraday

import starling.db.IntradayTradeSystem
import java.lang.String
import starling.richdb.{RichInstrumentResultSetRow, RichDB}
import starling.instrument.TradeableType
import starling.manager.Broadcaster
import starling.pivot._
import starling.daterange.{Day, Timestamp}
import starling.eai.{TreeID, EAIStrategyDB}
import starling.auth.{User, LdapUserLookup}
import starling.utils.Log
import starling.instrument.{Trade, TradeID, TradeAttributes}
import starling.dbx.QueryBuilder._
import starling.tradestore.eai.ExternalSortIndexPivotTreePathOrdering
import starling.tradestore.TradeStore.StoreResults
import starling.dbx.{QueryBuilder, Clause, Query}
import starling.gui.api.{Desk, IntradayUpdated}
import starling.tradeimport.ClosedDesks
import starling.tradestore.{RichTradeStore}
import scalaz.Scalaz._

case class IntradayTradeAttributes(strategyID: Option[TreeID], bookID: TreeID, dealID: Option[TreeID],
                                   trader: String, tradedFor: String, broker: String, comment: String, clearingHouse: String,
                                   subgroupName: String, entryDate: Day, username:String) extends TradeAttributes {
  import IntradayTradeAttributes._

  private lazy val desk = Desk.eaiDeskFromID(bookID.id).fold(_.name, bookID.id.toString)

  def persistedDetails = Map(
    "Book ID" -> bookID.id,
    strategyID_str -> (if (strategyID.isEmpty) TreeID(0) else strategyID.get),
    dealID_str -> (if (dealID.isEmpty) TreeID(0) else dealID.get),
    trader_str -> trader,
    tradedFor_str -> tradedFor,
    broker_str -> broker,
    clearing_str -> clearingHouse,
    comment_str -> comment,
    subgroupName_str -> subgroupName,
    "EntryDate" -> entryDate,
    username_str -> username
  )

  override def createFieldValues = {
    Map(
      Field(desk_Str) -> desk,
      //Strategy is intentionally excluded as it needs to hold the path, not just the id, so it is added in #joiningTradeAttributeFieldValues
      Field(trader_str) -> trader,
      Field(tradedFor_str) -> tradedFor,
      Field(broker_str) -> broker,
      Field(comment_str) -> comment,
      Field(clearing_str) -> clearingHouse,
      Field(subgroupName_str) -> subgroupName,
      Field("Entry Date") -> entryDate,
      Field(username_str) -> username
    ) ++ dealID.map(id => Map(Field(dealID_str) -> id)).getOrElse(Map())
  }
}

object IntradayTradeAttributes {
  val bookID_str = "BookID"
  val desk_Str = "Desk"
  val strategyID_str = "StrategyID"
  val dealID_str = "Deal ID"
  val trader_str = "Trader"
  val tradedFor_str = "Traded For"
  val broker_str = "Broker"
  val clearing_str = "Clearing House"
  val comment_str = "Comment"
  val subgroupName_str = "Subgroup Name"
  val entryDate_str = "Entry Date"
  val username_str = "Username"

}

/**
 * Trades stored intra-day.
 */
class IntradayTradeStore(
        db: RichDB,
        eaiStrategyDB: EAIStrategyDB,
        broadcaster: Broadcaster,
        ldapSearch: LdapUserLookup,
        closedDesks: ClosedDesks
        ) extends RichTradeStore(db, IntradayTradeSystem, closedDesks) {

  import IntradayTradeAttributes._

  def deskOption : Option[Desk] = None
  def createTradeAttributes(row: RichInstrumentResultSetRow) = {
    val strategyID = row.isNull(strategyID_str) match {
      case true => None
      case false => Some(TreeID(row.getInt(strategyID_str)))
    }
    val bookID = row.isNull(bookID_str.replaceAll(" ", "")) match {
      case true => TreeID(-1)
      case false => TreeID(row.getInt(bookID_str.replaceAll(" ", "")))
    }
    val dealID = row.isNull(dealID_str.replaceAll(" ", "")) match {
      case true => None
      case false => Some(TreeID(row.getInt(dealID_str.replaceAll(" ", ""))))
    }
    val trader = row.getString(trader_str)
    val tradedFor = row.getString(tradedFor_str.replaceAll(" ", ""))
    val broker = row.getString(broker_str)
    val clearer = row.getString(clearing_str.replaceAll(" ", ""))
    val comment = row.getString(comment_str)
    val subgroupName = row.getString(subgroupName_str.replaceAll(" ", ""))
    val entryDate = row.getDay(entryDate_str.replaceAll(" ", ""))
    val username = row.getString(username_str)

    new IntradayTradeAttributes(strategyID, bookID, dealID, trader, tradedFor, broker, comment, clearer, subgroupName, entryDate,
      username)
  }

  override def joiningTradeAttributeFieldValues(tradeAttributes:TradeAttributes) = {
    tradeAttributes.asInstanceOf[IntradayTradeAttributes].strategyID match {
      case Some(id) => Map(Field("Strategy") -> eaiStrategyDB.pathFor(id))
      case None => Map()
    }
  }

  override val tradeAttributeFieldDetails = {
    new StrategyFieldDetails(new ExternalSortIndexPivotTreePathOrdering(eaiStrategyDB)) ::
    List(desk_Str, dealID_str, tradedFor_str, trader_str, broker_str, clearing_str, comment_str, subgroupName_str,
      entryDate_str, username_str).map(n=>FieldDetails(n))
  }

  def pivotDrillDownGroups() = {
    List(
      DrillDownInfo(PivotAxis( List(), List(), List(), false)),
      DrillDownInfo(PivotAxis( List(), List(Field("Instrument"), Field("Market")), List(), false)),
      instrumentFilteredDrillDown
    )
  }

  def pivotInitialState(tradeableTypes:Set[TradeableType[_]]) = {
    val validFields = Set() ++ tradeableTypes.flatMap(_.fields)
    val blotterFields = List("Trade Day", "Quantity", "Market", "Instrument", "Strike", "Call Put", "Period", "Exercise Type",
      "Premium", "Counterparty", "Broker", "Clearing House", "Trader", "Traded For", "Comment", "Entry Date")
    val pfs = PivotFieldsState(
      rowFields = List(Field("Trade ID")),
      dataFields = blotterFields.filter(validFields.contains).map(n=>Field(n)),
      filters = List((Field("Strategy"), AllSelection))
    )
    DefaultPivotState(pfs)
  }

  protected def closesFrom(from:Timestamp, to:Timestamp) = List(to)

  val tableName = "IntradayTrades"

  /**
   * gets the distinct list of subgroup names, so that it can be displayed for the GUI
   * user to choose between.
   */
  def intradayLatest: Map[String, (User, Timestamp)] = {
    import QueryBuilder._

    db.queryWithResult(select("subgroupName, username, max(timestamp) ts")
            from tableName
            where ("subgroupName" isNotNull)
            groupBy(List("subgroupName", "username"))
      ) {
      rs => {
        val user = rs.getString("username") match {
          case null => User.Test
          case u => ldapSearch.user(u) match {
            case Some(u) => u
            case None => {
              Log.error("Invalid user in DB. Entries belonging to this user will have to be deleted")
              User.Test
            }
          }
        }
        (rs.getString("subgroupName"), (user, rs.getTimestamp("ts")))
      }
    }.toMap
  }


  def storeTrades(user : User, subgroupName: String, trades : List[Trade]): StoreResults = {
    val timestamp = new Timestamp
    val store = storeTrades({trade => {
      trade.attributes match {
        case a: IntradayTradeAttributes => a.subgroupName.equalsIgnoreCase(subgroupName)
        case _ => throw new Exception("Bad trade attributes: " + trade)
      }
    }}, trades, timestamp)

    if (store.changed) {
      broadcaster.broadcast(IntradayUpdated(subgroupName, user, intradayLatest(subgroupName)._2))
    }

    store
  }

}
