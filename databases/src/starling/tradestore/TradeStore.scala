package starling.tradestore


import starling.utils.sql._
import starling.pivot.{Field => PField}
import starling.pivot._
import starling.pivot.Field._
import starling.utils.ImplicitConversions._
import starling.richdb.{RichInstrumentResultSetRow, RichDB}
import starling.dbx.QueryBuilder._
import starling.instrument._
import starling.pivot._
import java.lang.String
import starling.utils.{Log}
import collection.{Iterable, Seq}
import starling.daterange._
import starling.instrument.{TradeID, Trade, TradeSystem, TradeAttributes}
import starling.gui.api._
import starling.tradestore.TradeStore.StoreResults
import concurrent.stm._
import collection.immutable._
import starling.db.{DBWriter}
import scalaz.Scalaz._
import starling.tradeimport.ClosedDesks


case class TradeRow(id: Int, trade: Trade)

/**
 * A wrapper around the starling Trade tables.
 */
abstract class TradeStore(db: RichDB, tradeSystem: TradeSystem, closedDesks: ClosedDesks) {
  private def defaultFrom(to: Timestamp = new Timestamp(Long.MaxValue)) = {
    val allCloses = closesFrom(new Timestamp(0), to)
    val default = if (allCloses.length > 7) {
      allCloses(allCloses.length - 7)
    } else {
      allCloses.min
    }
    default
  }

  private def latestToAvailable = maxTimestamp
  private val earliestExpiryDay: Ref[Option[Day]] = Ref(Option(Day.today.startOfFinancialYear))
  protected val versions: Ref[TreeMap[Timestamp, Map[TradeID, TradeRow]]] = Ref(new TreeMap[Timestamp, Map[TradeID, TradeRow]]())

  def deskOption: Option[Desk]

  val tradeAttributeFieldDetails: List[FieldDetails] = List()

  def createTradeAttributes(row: RichInstrumentResultSetRow): TradeAttributes

  def joiningTradeAttributeFieldValues(tradeAttributes: TradeAttributes): Map[PField, Any] = Map()

  def tradeAttributeFieldsAsSQLColumnNames = tradeAttributeFieldDetails.map(_.field.name.removeWhiteSpace.toLowerCase)

  lazy val allPossibleFieldDetails = Map() ++ (tradeableFieldDetails ++ tradeAttributeFieldDetails ++ JustTradeFields.fieldDetails).map {
    fd => fd.field -> fd
  }

  def tradesChanged() = {}

  val tableName: String

  def tradeCount = readLatestVersionOfAllTrades().size

  private def trades(sql: String, map: Map[String, Any], curr: Map[TradeID, TradeRow]) = {
    var res = curr
    db.query(sql, map) {
      rs => {
        val timestamp = rs.getTimestamp("timestamp")
        val id = rs.getInt("ID")
        val trade = TradeStore.tradeFromRow(rs, tradeSystem, createTradeAttributes(rs))
        if (trade.isDeletedTrade) {
          res -= trade.tradeID
        } else {
          res += (trade.tradeID -> TradeRow(id, trade))
        }
      }
    }
    res
  }

  protected def closesFrom(from: Timestamp, to: Timestamp): List[Timestamp]

  private def refresh(to: Timestamp, expiry: Option[Day]): Unit = atomic {
    implicit txn => {
      val defaultStart = defaultFrom(to)
      val from = versions().keySet.headOption match {
        case Some(v) if v < defaultStart => v
        case _ => defaultStart
      }
      refresh(from, to, expiry)
    }
  }

  private def refresh(from: Timestamp, to: Timestamp, expiry: Option[Day]): Unit = atomic {
    implicit txn => {
      assert(from <= to, "From timestamp needs to be on or before to: " +(from, to))

      val needToRefreshBecauseOfRequestedOlderExpiry = (earliestExpiryDay(), expiry) match {
        case (Some(d1), Some(d2)) => d2 < d1
        case (Some(_), None) => true
        case (None, Some(_)) => false
        case _ => false
      }

      val currentFrom = versions().keySet.headOption
      val currentTo = versions().keySet.lastOption

      val fetchOlder = needToRefreshBecauseOfRequestedOlderExpiry || currentFrom.fold(from < _, true)
      val fetchNewer = needToRefreshBecauseOfRequestedOlderExpiry || currentTo.fold(to > _, true)
      val endTimestamp = latestToAvailable

      if (endTimestamp.isDefined && (fetchNewer || fetchOlder)) {
        val lastTimestamp = endTimestamp.get max to

        var params: Map[String, Any] = Map[String, Any]()
        val (expStr, actualExpiry) = expiry match {
          case Some(d) => {
            // always do start of financial year then filter anything unneeded out later. this stops us
            // from doing a query then having to refresh it a few seconds later when someone runs a report.
            val day = d.startOfFinancialYear
            params += ("expDay" -> day)
            val query = " and (t.expiryDay_cache >= :expDay or (t.instrument = 'Error Instrument' or t.instrument = 'DeletedInstrument'))"
            (query, Some(day))
          }
          case None => ("", expiry)
        }

        val startTimestamp = if (fetchOlder) {
          Log.infoWithTime("Refreshing all trade from " + from) {
            // we can optimise this if needed but for now if we have to go backwards just blank the trade history
            versions() = new TreeMap[Timestamp, Map[TradeID, TradeRow]]()
            val latest: Map[TradeID, TradeRow] = Map[TradeID, TradeRow]()
            params += ("ts" -> from)
            val bootstrap = trades("""
              select * from """ + tableName + """ t
              where
                  timestamp <= :ts
                  and (timestampTo_cache > :ts or timestampTo_cache is null)
                  """ + expStr,
              params,
              latest
            )
            versions() = versions() + (from -> bootstrap)
            earliestExpiryDay() = actualExpiry
            from
          }
        } else {
          currentTo.get
        }

        if (lastTimestamp > startTimestamp) {
          Log.infoWithTime("Building trade history after " + startTimestamp) {

            val closes = closesFrom(startTimestamp, lastTimestamp).toIterator
            def nextCloseOrLatest = closes.hasNext match {
              case true => closes.next
              case false => lastTimestamp
            }
            var current = nextCloseOrLatest
            val sql = "select * from " + tableName + """ t
                                  where timestamp > :old
                                  """ + expStr + " order by id asc"

            var map = versions().apply(startTimestamp)
            db.query(sql, params + ("old" -> startTimestamp)) {
              rs => {
                val timestamp = rs.getTimestamp("timestamp")
                val id = rs.getInt("ID")
                val trade = TradeStore.tradeFromRow(rs, tradeSystem, createTradeAttributes(rs))

                if (timestamp > current) {
                  do {
                    versions() = versions() + ((current -> map))
                    current = nextCloseOrLatest
                  } while (current < timestamp)
                }

                if (trade.isDeletedTrade) {
                  map -= trade.tradeID
                } else {
                  map += (trade.tradeID -> TradeRow(id, trade))
                }
              }
            }
            do {
              versions() = versions() + ((current -> map))
              current = nextCloseOrLatest
            } while (current < lastTimestamp)
            versions() = versions() + ((lastTimestamp -> map))
          }
        }
      }
    }
  }

  def latestKnownTimestamp: Option[Timestamp] = latestToAvailable

  /**
   * Usages of this have no interest in expiry day (e.g. user enters trade id in gui), so we need to
   * read all versions of this trade.
   */
  def readTrade(tradeID: TradeID, timestamp: Option[Timestamp] = None): Option[TradeRow] = {
    val history = singTradeHistory(tradeID)

    val relevantHistory = timestamp.fold(ts => history.filterKeys(_ <= ts), history)

    relevantHistory.lastOption.map(_._2)
  }

  def singTradeHistory(tradeID: TradeID): TreeMap[Timestamp, TradeRow] = {
    val res = db.queryWithResult("select * from " + tableName + " where tradeID = :tradeID order by id asc", Map("tradeID" -> tradeID.id)) {
      rs => {
        val timestamp = rs.getTimestamp("timestamp")
        val id = rs.getInt("ID")
        val trade = TradeStore.tradeFromRow(rs, tradeSystem, createTradeAttributes(rs))
        (timestamp -> TradeRow(id, trade))
      }
    }
    new TreeMap[Timestamp, TradeRow]() ++ res.toMap
  }

  protected def atVersion(timestamp:Timestamp)= atomic {
    implicit txn => {
      versions().apply(timestamp)
    }
  }

  def readLatestVersionOfAllTrades(expiry: Option[Day] = None): Map[TradeID, TradeRow] = atomic {
    implicit txn => {
      val newest = versions().keySet.maxOr(new Timestamp(0))
      latestToAvailable match {
        case Some(ts) => {
          if (ts > newest) {
            refresh(ts, expiry)
          }
          atVersion(ts)
        }
        case _ => Map.empty
      }
    }
  }

  def readAll(timestamp: Timestamp, expiryDay: Option[Day], marketDay: Option[Day]): Map[TradeID, TradeRow] = atomic {
    implicit txn => {
      refresh(timestamp, expiryDay)
      val trades = atVersion(timestamp)
      if (marketDay.isDefined || expiryDay.isDefined) {
        Log.infoWithTime("Filtering trades in readAll") {
          trades.filter {
            case (_, TradeRow(_, trade)) => {
              marketDay.fold(trade.tradeDay <= _, true) && ((trade.expiryDay, expiryDay) match {
                case (Some(d1), Some(d2)) => d1 >= d2
                case (Some(_), None) => true
                case (None, Some(_)) => true
                case _ => true
              })
            }
          }
        }
      } else {
        trades
      }
    }
  }

  def tradeableFieldDetails = TradeableFields.fieldDetails

  /**
   * gets the largest ID of any trade in the named subgroup, or 0 if there are no trades. this assumes that the IDs are
   * strictly increasing, which is probably always true.
   */
  protected def tradesHash(predicate: (Trade) => Boolean): Long = {
    (0 :: readLatestVersionOfAllTrades().filter(v => predicate(v._2.trade)).map(_._2.id).toList).max
  }

  private def maxTimestamp(): Option[Timestamp] = {
    val q = (
      select("max(timestamp) m")
        from (tableName + " t")
      )
    val res = db.queryWithOneResult(q) {
      row => {
        if (row.isNull("m")) None
        else Some(row.getTimestamp("m"))
      }
    }
    res.flatMap(identity)
  }

  protected def allTimestamps:List[Timestamp] = {
    val q = (
      select("distinct(timestamp) ts")
        from (tableName + " t")
        orderBy ("ts" asc)
      )
    val res = db.queryWithResult(q) {
      row => {
        if (row.isNull("ts")) None
        else Some(row.getTimestamp("ts"))
      }
    }
    res.flatten
  }

  private def inTransaction(f: Writer => Unit) {
    db.inTransaction(java.sql.Connection.TRANSACTION_READ_COMMITTED) {
      dbWriter => dbWriter.withIdentityInsert(tableName) {
        new Writer(dbWriter).flushToDB(f)
      }
    }
  }

  def storeTrades(predicate: (Trade) => Boolean, trades: Iterable[Trade], timestamp: Timestamp): StoreResults = this.synchronized {
    var result: StoreResults = null
    inTransaction(writer => {
      val expiry = timestamp.day.startOfFinancialYear
      val allTrades = readLatestVersionOfAllTrades(Some(expiry))
      val incomingTrades = trades.filter(_.expiryDay.fold(_ >= expiry, true))
      val currentTrades = allTrades.filter(v => predicate(v._2.trade))
      val (added, updated, _) = writer.updateTrades(incomingTrades, currentTrades, timestamp)

      val currentTradeIDs = currentTrades.map(_._1)
      // the update step above doesn't remove trades, so this needs to be done as a separate step
      val updateTradeIDs = Set[TradeID]() ++ incomingTrades.map(_.tradeID)
      val deletedTradeIDs = currentTradeIDs.filterNot(id => updateTradeIDs.contains(id)).toSet
      writer.delete(deletedTradeIDs, timestamp)

      Log.info("Deleting " + deletedTradeIDs.size + " trades. " +(allTrades.size, currentTrades.size, incomingTrades.size, added, updated))

      val hash = tradesHash(predicate)
      result = StoreResults(added, deletedTradeIDs.size, updated, hash)
    })

    assume(result != null, "Something went wrong storing the trades: " + trades) // sanity check

    if(result.changed)
      tradesChanged

    result
  }

  class Writer(writer: DBWriter) {

    private def findMaxID = {
      val q = (
        select("max(id) i")
          from (tableName + " t")
        )
      db.queryWithOneResult(q) {
        row => {
          if (row.isNull("i")) 0 else row.getInt("i")
        }
      }.get
    }

    var currentID = findMaxID

    private def nextID = {
      currentID += 1
      currentID
    }

    private def createTableValues(trade: Trade, timestamp: Timestamp) = {
      val instrument = trade.tradeable
      val expiryDay = instrument.expiryDay match {
        case Some(day) => day
        case None => {
          assert(instrument.isInstanceOf[ErrorInstrument] || instrument.isInstanceOf[DeletedInstrument], "No expiry error: " + instrument)
          null
        }
      }

      val justTradeDetails = Map(
        tradeID_str -> trade.tradeID,
        tradeDay_str -> trade.tradeDay,
        counterparty_str -> trade.counterParty,
        instrument_str -> trade.tradeable.tradeableType.name,
        costs_str -> PersistAsBlob(trade.costs)
      )

      val details = Map(
        "Id" -> nextID,
        "timestamp" -> timestamp,
        "expiryDay_cache" -> expiryDay
      ) ++ justTradeDetails ++ trade.tradeable.persistedTradeableDetails ++ trade.attributes.details

      val normalisedNames = details.map {
        case (field, value) => field.removeWhiteSpace -> value
      }

      normalisedNames
    }

    private var toInsertTradeTable = List[Map[String, Any]]()

    private def insert(params: Map[String, Any]) = {
      toInsertTradeTable ::= params
      if (toInsertTradeTable.size > 100) {
        flushToDB
      }
    }

    def flushToDB[A](f: Writer => A): A = {
      val result = f(this)
      flushToDB
      result
    }

    def flushToDB = {
      writer.insert(tableName, toInsertTradeTable)
      toInsertTradeTable = List[Map[String, Any]]()
    }

    private def insertNewTrade(trade: Trade, timestamp: Timestamp): Int = {
      assert(trade.tradeID.tradeSystem == tradeSystem, "Importing trade from " + trade.tradeID.tradeSystem + " into " + tradeSystem)
      val details = createTableValues(trade, timestamp)
      val id = details.get("Id") match {
        case Some(id: Int) => id
        case _ => throw new Exception("Shouldn't happen")
      }
      insert(details)
      id
    }

    private def writeUpdatedTrade(trade: Trade, timestamp: Timestamp, previousID: Int) {
      val id = insertNewTrade(trade, timestamp)
      writer.update(tableName, Map("nextVersionId_cache" -> id, "timestampTo_cache" -> timestamp), ("id" eql previousID))
    }

    def abort = writer.abort

    def updateTrades(updateTrades: Iterable[Trade],
                     oldTrades: Map[TradeID, TradeRow],
                     timestamp: Timestamp) = {

      var added = 0
      var updated = 0
      var same = 0
      var total = 0
      for (updateTrade <- updateTrades) {
        oldTrades.get(updateTrade.tradeID) match {
          case Some(oldTradeRow) => oldTradeRow.trade == updateTrade match {
            case false => {
              if (oldTradeRow.trade.toString == updateTrade.toString) {
                // This is usually, although not always, a warning sign that we've implemented trade comparison incorrectly
                // and we're producing new versions of trades that haven't actually changed.
                Log.warn("Trades apparently not equal but their strings are the same: " + updateTrade)
              }
              writeUpdatedTrade(updateTrade, timestamp, oldTradeRow.id)
              updated += 1
            }
            case true => {
              same += 1
            }
          }
          case None => {
            insertNewTrade(updateTrade, timestamp)
            added += 1
          }
        }
        total += 1
      }
      flushToDB
      assume(added + updated + same == total)
      (added, updated, same)
    }

    def delete(tradeIDs: Set[TradeID], timestamp: Timestamp) {
      val latestVersionOfTrades = readLatestVersionOfAllTrades().filter {
        case (id, _) => tradeIDs.contains(id)
      }
      delete(tradeIDs, latestVersionOfTrades, timestamp)
    }

    /**
     * Delete all the tradeids.
     *
     * Works by adding new DeletedInstruments to the trade table.
     */
    def delete(tradeIDs: Set[TradeID], oldTrades: Map[TradeID, TradeRow], timestamp: Timestamp = new Timestamp) {
      for (tradeID <- tradeIDs) {
        oldTrades.get(tradeID) match {
          case Some(oldTradeRow: TradeRow) if !oldTradeRow.trade.isDeletedTrade => {
            writeUpdatedTrade(oldTradeRow.trade.copyWithInstrument(new DeletedInstrument), timestamp, oldTradeRow.id)
          }
          case _ =>
        }
      }
      flushToDB
    }
  }

}

object TradeStore {

  case class StoreResults(inserted: Int, deleted: Int, updated: Int, hash: Long) {
    def changed = inserted > 0 || deleted > 0 || updated > 0
  }

  def tradeFromRow(row: RichInstrumentResultSetRow, tradeSystem: TradeSystem, tradeAttributes: TradeAttributes): Trade = {
    try {
      val instrumentName = row.getString("Instrument")
      val instrumentType = TradeableType.fromName(instrumentName)
      val instrument = instrumentType.createTradeable(row)
      val tradeID = TradeID(row.getString("tradeID"), tradeSystem)
      val tradeDay = row.getDay("TradeDay")
      val counterparty = row.getString("counterparty")
      val costs: List[Costs] = row.getObjectOrElse("costs", List())
      Trade(tradeID, tradeDay, counterparty.intern, tradeAttributes, instrument, costs)
    } catch {
      case e: Throwable => throw new Exception(e.getMessage + ", " + e.getClass + " in row " + row.toString, e)
    }
  }
}

