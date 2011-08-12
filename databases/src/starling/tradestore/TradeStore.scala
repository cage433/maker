package starling.tradestore

import starling.utils.sql._
import starling.utils._
import starling.pivot.{Field => PField}
import starling.pivot._
import controller.PivotTableConverter
import model.{UndefinedValue, CollapsedState, PivotTableModel}
import starling.pivot.Field._
import starling.db.DBWriter
import collection.immutable.Set
import starling.utils.cache.CacheFactory
import starling.utils.ImplicitConversions._
import starling.richdb.{RichInstrumentResultSetRow, RichDB}
import starling.utils.sql.QueryBuilder._
import starling.utils.sql.LiteralString
import java.util.concurrent.atomic.AtomicReference
import collection.immutable.List
import starling.instrument._
import starling.pivot._
import java.lang.String
import starling.utils.{Broadcaster, AppendingMap, Log}
import collection.{Iterable, Seq}
import starling.daterange._
import starling.trade.{TradeID, Trade, TradeSystem, TradeAttributes}
import math.Ordering
import starling.quantity._
import starling.gui.api._
import starling.tradestore.TradeStore.StoreResults
import starling.marketdata.PeriodFieldDetails

//This is the code which maps from TradeableType.fields to FieldDetails
object TradeableFields {

  private val fieldDetailsAndMapper:Seq[(FieldDetails,(Trade,Any)=>Any)] = TradeableType.fieldsWithType.map { case(name,klass) => {
    val dateRange = classOf[DateRange]
    val quantity = classOf[Quantity]
    val spreadOrQuantity = classOf[SpreadOrQuantity]
    val period = classOf[Period]
    (name, klass) match {
      case ("Quantity", _) => (new TradeIDGroupingSumPivotQuantityFieldDetails("Quantity"), (t:Trade,q:Any)=>Map(t.tradeID.toString -> q))
      case (_, `dateRange`) => (new PeriodFieldDetails(name), (t:Trade,v:Any)=>{
        v match {
          case d:DateRange => DateRangePeriod(d)
          case s:Spread[_] => SpreadPeriod(s.first, s.last)
          case s:Strip[_] => StripPeriod(s.first, s.last)
          case p:Period => p
          case _ => throw new Exception(v + " is not a DateRange or Period")
        }
      })
      case (_, `spreadOrQuantity`) => (new PivotSpreadQuantityFieldDetails(name), (t:Trade,v:Any)=> {
        v match {
          case q:Quantity => SpreadOrQuantity(Left(q))
          case sp:SpreadQuantity => SpreadOrQuantity(Right(sp))
          case s:SpreadOrQuantity => s
        }
      })
      case (_, `period`) => (new PeriodFieldDetails(name), (t:Trade,p:Any)=>p)
      case (_, `quantity`) => (new QuantityLabelFieldDetails(name), (t:Trade,q:Any) => q)
      case (_,_) => (FieldDetails(name), (t:Trade,v:Any)=>v.toString.asInstanceOf[Any])
    }
  } }
  private val normalizedNameToFieldAndMapper:Map[String,(FieldDetails,(Trade,Any)=>Any)] = Map() ++ fieldDetailsAndMapper.map{t=>t._1.field.name.removeWhiteSpace.toLowerCase->t}

  val fieldDetails = fieldDetailsAndMapper.map(_._1).toList
  val fields = fieldDetails.map (_.field)

  def createFieldValues(trade:Trade, tradeable:Tradeable):Map[PField,Any] = {
    val tradeableDetails : Map[String, Any] = tradeable.shownTradeableDetails
    tradeableDetails.map { case (k, v) => {
      val (fieldDetails, mapper) = normalizedNameToFieldAndMapper(k.toLowerCase.removeWhiteSpace)
      (fieldDetails.field, mapper(trade,v))
    }}
  }
}

//This is the code which defines and creates the Trade Fields (not the Tradeable or TradeAttribute fields)
object JustTradeFields {
  def createFieldValues(trade:Trade) = {
    val idAsLabel = TradeIDLabel(trade.tradeID.id, TradeSystemLabel(trade.tradeID.tradeSystem.name, trade.tradeID.tradeSystem.shortCode))
    Map(
      PField("Trade Count") -> idAsLabel,
      PField("Trade ID") -> idAsLabel,
      PField("Trade Day") -> trade.tradeDay,
      PField("Counterparty") -> trade.counterParty,
      PField("Instrument") -> trade.tradeable.tradeableType.name
   ) ++ (trade.premium match {
      case Some(p) => Map(PField("Initial Price") -> SpreadOrQuantity(Left(p)))
      case None => Map()
    })
  }
  val fieldDetails = List(
    new FieldDetails("Trade Count") {
      override def isDataField = true
      override def transformValueForGroupByField(a: Any) = "Undefined"
      override def value(a: Any) = a
      override def formatter = SetSizePivotFormatter
    },
    new FieldDetails("Trade ID") {
      override def formatter = TradeIDPivotFormatter
    },
    FieldDetails("Trade Day"),
    FieldDetails("Counterparty"),
    FieldDetails("Instrument"),
    new QuantityLabelFieldDetails("Initial Price")
  )
  val fields = fieldDetails.map(_.field)

}

case class FieldDetailsTradeSelection(filter:List[(FieldDetails,Selection)], selection:List[List[(FieldDetails,Selection)]]) {
  def apply(data: Map[PField, Any]) = {
    def matches(fs: (FieldDetails, Selection)): Boolean = {
      try {
        val (fieldDetail, selection) = fs
        val value = data.getOrElse(fieldDetail.field, UndefinedValue)
        selection.matches(fieldDetail, value)
      } catch {
        case e => {
          println("data is ")
          data.foreach(println)
          throw e
        }
      }
    }
    filter.forall(matches) && (selection.isEmpty || selection.exists(_.forall(matches)))
  }
}
object FieldDetailsTradeSelection {
  val Null = FieldDetailsTradeSelection(List(), List())
}


case class TradeRow(id: Int, timestamp: Timestamp, trade: Trade)

case class TradeChanges(
        fields: List[FieldDetailsGroup] = List(),
        movedIn: List[TradeAndFields] = List(),
        movedOut: List[TradeAndFields] = List(),
        created: List[TradeAndFields] = List(),
        deleted: List[TradeAndFields] = List(),
        undeleted: List[TradeAndFields] = List(),
        amended: List[(TradeAndFields, TradeAndFields)] = List()) {
  def removeUntraded(marketDay: Day) = {
    //Maybe Trade should have no mtm before its trade day but that would be disruptive and requires some thought
    def filter(trades: List[TradeAndFields]) = trades.filter(r => r.trade.tradeDay <= marketDay)
    //There is still an assue with amendments to the trade day. For consistency this should be fixed in Trade
    TradeChanges(fields, filter(movedIn), filter(movedOut), filter(created), filter(deleted), filter(undeleted), amended)
  }
  def changes = movedIn.size + movedOut.size + created.size + deleted.size + undeleted.size + amended.size
}

/**
 * A wrapper around the starling Trade tables.
 */
abstract class TradeStore(db: RichDB, broadcaster:Broadcaster, tradeSystem: TradeSystem, bookID:Option[Int]) {

  lazy val cachedLatestTimestamp:AtomicReference[Timestamp] = new AtomicReference(maxTimestamp())

  val tradeAttributeFieldDetails:List[FieldDetails] = List()
  def createTradeAttributes(row: RichInstrumentResultSetRow): TradeAttributes
  def joiningTradeAttributeFieldValues(tradeAttributes:TradeAttributes):Map[PField,Any] = Map()

  def tradeAttributeFieldsAsSQLColumnNames = tradeAttributeFieldDetails.map(_.field.name.removeWhiteSpace.toLowerCase)


  lazy val allPossibleFieldDetails = Map() ++ (TradeableFields.fieldDetails ++ tradeAttributeFieldDetails ++ JustTradeFields.fieldDetails).map {
    fd => fd.field -> fd
  }

  private def maxTimestamp():Timestamp = {
    val q = (
            select("max(timestamp) m")
                    from (tableName + " t")
            )
    db.queryWithOneResult(q) {row => {if (row.isNull("m")) {new Timestamp(0)} else row.getTimestamp("m")}} match {
      case Some(ts) => ts
      case none => new Timestamp(0)
    }
  }

  def inTransaction(f: Writer => Unit) {
    inTransaction(java.sql.Connection.TRANSACTION_READ_COMMITTED)(f)
  }

  def inTransaction(isolationLevel: Int)(f: Writer => Unit) {
    val oldLatestTimestamp = cachedLatestTimestamp.get
    db.inTransaction(isolationLevel) {
      dbWriter => dbWriter.withIdentityInsert(tableName) {
        new Writer(dbWriter).flushToDB(f)
      }
    }
    val newMax = maxTimestamp
    cachedLatestTimestamp.compareAndSet(oldLatestTimestamp, newMax)
    if (newMax > oldLatestTimestamp) {
      importNewVersions(oldLatestTimestamp, newMax)
      tradesChanged()
    }
  }

  def tradesChanged() = {}

  val tableName: String

  def createFieldValues(trade:Trade) = {
    val justTradeDetails = JustTradeFields.createFieldValues(trade)
    val tradeAttributeFields = trade.attributes.createFieldValues
    val instrumentDetails = TradeableFields.createFieldValues(trade, trade.tradeable)
    new AppendingMap(Map("Trade" -> justTradeDetails, "TradeAtributes" ->tradeAttributeFields, "Instrument"->instrumentDetails))
  }

  val tradeHistories = new TradeHistories()

  private var initialLoadDone = false
  private var earliestTimestamp = new Timestamp()
  private var earliestExpiryDay:Option[Day] = Some(Day.today.previousWeekday)

  private def addTradeRowToHistory(rs : RichInstrumentResultSetRow) {
    val timestamp = rs.getTimestamp("timestamp")
    val id = rs.getInt("ID")
    val trade = TradeStore.tradeFromRow(rs, tradeSystem, createTradeAttributes(rs))
    tradeHistories.addTrade(id, timestamp, trade, createFieldValues(trade))
  }

  private def buildStandardQuery(timestamp:Timestamp, expiryDay:Option[Day], key:String):(String,Map[String,Any]) = {
    var sql = "select * from " + tableName + """ t
    where timestamp < :%s and (:%s < timestampTo_cache or timestampTo_cache is null)
    and t.instrument != 'Deleted Instrument'""" % (key, key) + bookID.map(c => " and bookid = " + c).getOrElse("")
    var params = Map[String,Any](key->timestamp)
    expiryDay match {
      case Some(d) => {
        sql += " and (t.expiryDay_cache >= :%s or t.instrument = 'Error Instrument')" % (key+"e")
        params += key+"e" -> d
      }
      case None =>
    }
    (sql, params)
  }

  private def initialLoad() {
    val query = buildStandardQuery(earliestTimestamp, earliestExpiryDay, "s")
    loadTrades(query)
  }

  private def importTradesVersionsBeforeTimestampAndAfterExpiryDay(timestamp: Timestamp, expiryDay:Option[Day]): Unit = {
    println("Updating for " + timestamp + ", " + expiryDay)
    val lastQuery = buildStandardQuery(earliestTimestamp, earliestExpiryDay, "last")
    val newQuery = buildStandardQuery(timestamp, expiryDay, "new")
    loadTrades( (newQuery._1 + " except " + lastQuery._1, newQuery._2 ++ lastQuery._2) )
  }


  private def loadTrades(query:(String,Map[String,Any])) {
    var results = List[(Int, Timestamp, Trade)]()
    Log.infoWithTime("Reading Trades") {
      db.query(query._1, query._2) {
        rs =>
          val timestamp = rs.getTimestamp("timestamp")
          val id = rs.getInt("ID")
          val trade = TradeStore.tradeFromRow(rs, tradeSystem, createTradeAttributes(rs))
          results = (id, timestamp, trade) :: results
      }
    }

    Log.infoWithTime("Adding " + results.size + " to history"){
      results.foreach{
        case (id, timestamp, trade) => tradeHistories.addTrade(id, timestamp, trade, createFieldValues(trade))
      }
    }
  }

  private def importOldVersions(from: Timestamp, to: Timestamp) {
    val query = (
      select ("*")
        from (tableName + " t")
        where (("t.timestamp" gte from) and ("t.timestamp" lt to) andMaybe (bookID.map(id => ("t.bookid" eql id))))
      )
    db.query(query)(addTradeRowToHistory)
  }

  private def importNewVersions(from: Timestamp, to: Timestamp) {
    val query = (
      select ("*")
      from (tableName + " t")
      where (("t.timestamp" gt from) and ("t.timestamp" lte to) andMaybe (bookID.map(id => ("t.bookid" eql id))))
    )
    db.query(query)(addTradeRowToHistory)
  }

  def readAll() {
    updateTradeHistories(Timestamp(0), None)
  }

  def tradeCount = tradeHistories.latestTradeRows.size

  val lock = new Object

  def isLoaded() = lock.synchronized { initialLoadDone }

  /**
   * Ensure that we have all trade versions after the given timestamp, and also the latest version before the timestamp,
   * for all trades that are live on or after the given expiry day
   */
  private def updateTradeHistories(timestamp : Timestamp, expiryDay : Option[Day]) {
    lock.synchronized {

      if (!initialLoadDone) {
        Log.infoWithTime("Updating trades as of " + timestamp + ", expiry day " + expiryDay) {
          initialLoad()
        }
        initialLoadDone = true
      }

      val minExpiryDay =
        (expiryDay, earliestExpiryDay) match {
          case (_, None) => None
          case (None, _) => None
          case (Some(ed), Some(eed)) => Some(ed min eed)
        }
      //read the latest version of trades which expired between expiryDay and earliestExpiryDay
      if (minExpiryDay != earliestExpiryDay || timestamp < earliestTimestamp) {
        importTradesVersionsBeforeTimestampAndAfterExpiryDay(timestamp min earliestTimestamp, minExpiryDay)
        if (timestamp < earliestTimestamp) {
          //read changes to trades which have already been read
          Log.info("Doing another quick update " + timestamp + " < " + earliestTimestamp)
          importOldVersions(timestamp, earliestTimestamp) //picks up amendments between timestamp and earliestTimestamp
        }
        earliestExpiryDay = minExpiryDay
        earliestTimestamp = timestamp min earliestTimestamp
      }
    }
  }

  private def updateFullHistoryForTrade(tradeID: TradeID): Unit = {
    new Object().synchronized {
      val q = (select("*")
        from (tableName + " t")
        where ("tradeid" eql LiteralString(tradeID.id))
        )
      db.query(q)(addTradeRowToHistory)
    }
  }

  private def getHistoryOrNone(tradeID : TradeID) = {
    tradeHistories.versionedTrades.get(tradeID)
  }

  private def getHistory(tradeID : TradeID) = getHistoryOrNone(tradeID).get

  /**
   * Usages of this have no interest in expiry day (e.g. user enters trade id in gui), so we need to
   * read all versions of this trade.
   */
  def readTrade(tradeID: TradeID, timestamp: Option[Timestamp] = None): Option[Trade] = {
    updateFullHistoryForTrade(tradeID)
    getHistoryOrNone(tradeID) match {
      case None => None
      case Some(history) => history.version(timestamp).map(_.trade)
    }
  }

  def init() {
    updateTradeHistories(earliestTimestamp, earliestExpiryDay)
  }

  def readLatestVersionOfAllTrades(): Map[TradeID, TradeRow] = {
    updateTradeHistories(earliestTimestamp, None)
    tradeHistories.latestTradeRows
  }

  def allTradesForTests(timestamp: Timestamp)(fn: Trade => Unit) {
    updateTradeHistories(earliestTimestamp, earliestExpiryDay)
    tradeHistories.tradesAsOf(timestamp, timestamp.day, FieldDetailsTradeSelection.Null).foreach(fn)
  }

  def allTradesForTests(tradePredicate: TradePredicate, timestamp: Timestamp)(fn: Trade => Unit) {
    updateTradeHistories(earliestTimestamp, earliestExpiryDay)
   tradeHistories.tradesAsOf(timestamp, Day(2001, 1, 1), fromPredicate(tradePredicate)).foreach{ fn }
  }

  def selectLiveAndErrorTrades(tradePredicate : TradePredicate, marketDay : Day, timestamp:Timestamp) = {
    updateTradeHistories(timestamp, Some(marketDay))
    tradeHistories.tradesAsOf(timestamp, marketDay, fromPredicate(tradePredicate))
  }

  def readAll(timestamp:Timestamp, tradePredicate : TradePredicate, expiryDay: Option[Day]) = {
    updateTradeHistories(timestamp, None)
    val trades = tradeHistories.tradeRowsAsOf(timestamp, expiryDay)
    val filter = fromPredicate(tradePredicate)
    var allTradeableTypes = new scala.collection.mutable.HashSet[TradeableType[_]]()
    val rows = trades.flatMap { tradeAndFields =>
      val joinedTradeAttributeDetails = joiningTradeAttributeFieldValues(tradeAndFields.trade.attributes)
      val tradeFields = new AppendingMap(Map("Join"->joinedTradeAttributeDetails) ++ tradeAndFields.fields.maps)
      if (filter(tradeFields)) {
        allTradeableTypes += tradeAndFields.trade.tradeable.tradeableType
        Some(TradeAndFields(tradeAndFields.id, tradeAndFields.trade, tradeFields))
      } else {
        None
      }
    }
    val fieldDetails = createFieldDetailGroups(allTradeableTypes.toSet)
    (fieldDetails, rows)
  }

  def readLatestVersionOfTrades(tradeIDs: Set[TradeID]): Map[TradeID, TradeRow] = {
    readLatestVersionOfAllTrades.filterKeys(tradeIDs)
  }

  def tradeHistory(tradeID: TradeID):Option[(STable,List[FieldDetailsGroup], List[CostsLabel])] = {
    updateFullHistoryForTrade(tradeID)
    getHistoryOrNone(tradeID).map { history => {
      val allTradeableTypes = Set() ++ history.versions.map { case (_, tradeAndFields) => tradeAndFields.trade.tradeable.tradeableType }
      val costsVersions = history.versions.map {case (_,tradeAndFields) => {
        val costs = tradeAndFields.trade.costs
        CostsLabel(costs.map(cost => {
          val value = cost match {
            case commission:CommissionCosts => commission.commission
            case premium:PremiumCosts => premium.premium
            case ordinary: OrdinaryCost => ordinary.quantity
            case _ => Quantity.NULL
          }
          CostsInfo(cost.costType, value, cost.info)
        }))
      }}
      val mapList:List[Map[PField,Any]] = (for (((timestamp, tradeAndFields), version) <- history.versions.toMap.zipWithIndex) yield {
        Map( PField("Version") -> (version+1), PField("Import Time")->timestamp) ++ tradeAndFields.fields ++ joiningTradeAttributeFieldValues(tradeAndFields.trade.attributes)
      }).toList
      val fieldDetailsGroups0 = createFieldDetailGroups(allTradeableTypes)
      val ds = new UnfilteredPivotTableDataSource() {
        val fieldDetailsGroups = {
          FieldDetailsGroup("Magic", FieldDetails("Version"), FieldDetails("Import Time")) :: fieldDetailsGroups0
        }
        def unfilteredData(pfs: PivotFieldsState) = mapList
      }
      val measures = ds.fieldDetails.filter(fd => (fd.field.name != "Trade ID") && (fd.field.name != "Trade Count" && fd.field.name != "Version"))
      val pivotTable = PivotTableModel.createPivotTableData(ds, PivotFieldsState(rowFields=List(PField("Version")), dataFields=measures.map(_.field)))
      (new PivotTableConverter(OtherLayoutInfo(), pivotTable).toSTable("Trade History"),
              fieldDetailsGroups0, costsVersions.toList)
    } }
  }

  private def fromPredicate(tradePredicate:TradePredicate) = {
    def mapToFieldDetails(a:(PField,Selection)) = allPossibleFieldDetails(a._1) -> a._2
    FieldDetailsTradeSelection(tradePredicate.filter.map(mapToFieldDetails), tradePredicate.selection.map(_.map(mapToFieldDetails)))
  }


  def tradeChangesForTests(t1: Timestamp, t2: Timestamp) = tradeChanges(t1, t2, t1.day, TradePredicate(List(), List()))
  /**
   * Given two timestamps and a filter it identifies the changes in de al population:
   *  movedIn    the trade existed at time t1 but did not match the predicate
   *  movedOut   the trade matched the predicate at t1 but not at t2
   *  created    the trade did not exist at t1
   *  deleted    the trade existed and matchedval trad the predicate at t1 but at t2 it was deleted
   *  undeleted  the trade was deleted at t1 but was not deleted at t2
   *  amended    the trade a t1 is not the same as the trade at t2 This applies to any parameter not just valuation parameters
   */
  def tradeChanges(t1: Timestamp, t2: Timestamp, expiryDay:Day, tradePredicate : TradePredicate): TradeChanges = {
    def rowsFor(t:Timestamp) = {
      updateTradeHistories(t, None)
      tradeHistories.tradeRowsAsOf(t, None).map { row => {
        //Add join based fields like Strategy and 'Group Company'
        val fields = new AppendingMap(row.fields.maps + ("Joining"->joiningTradeAttributeFieldValues(row.trade.attributes)))
        val newRow = TradeAndFields(row.id, row.trade, fields)
        (row.trade.tradeID, newRow)
      }}.toMap
    }
    var t1Trades = rowsFor(t1)
    var t2Trades = rowsFor(t2)

    val tradeSelection = fromPredicate(tradePredicate)
    val tradeIds = (t1Trades.keySet ++ t2Trades.keySet).filter{
      tradeID =>
         List(t1Trades.get(tradeID), t2Trades.get(tradeID)).flatten.exists(r=>tradeSelection(r.fields))
    }
    t1Trades = t1Trades.filterKeys(tradeIds)
    t2Trades = t2Trades.filterKeys(tradeIds)

    val groupedt2Trades = t2Trades.groupBy{
      case (t2TradeId, t2TradeRow) =>
        t1Trades.get(t2TradeId) match{
          case None => "Created"
          case Some(t1TradeRow) => {
            if (t1TradeRow.trade == t2TradeRow.trade)
              "Unchanged"
            else if (t1TradeRow.trade.isDeletedTrade && !t2TradeRow.trade.isDeletedTrade)
              "Undeleted"
            else if (!t1TradeRow.trade.isDeletedTrade && t2TradeRow.trade.isDeletedTrade)
              "Deleted"
            else if (t1TradeRow.matches(tradeSelection) && ! t2TradeRow.matches(tradeSelection))
              "Moved Out"
            else if (! t1TradeRow.matches(tradeSelection) && t2TradeRow.matches(tradeSelection))
              "Moved In"
            else
              "Amended"
          }
        }
    }
    def valuesList(map : scala.collection.Map[TradeID, TradeAndFields]) = map.valuesIterator.toList

    val allTradeableTypes = Set() ++ t1Trades.values.map(_.trade.tradeable.tradeableType) ++ t2Trades.values.map(_.trade.tradeable.tradeableType)

    val deletedTrades = t1Trades.filterKeys(id => !t2Trades.contains(id))
    TradeChanges(
      createFieldDetailGroups(allTradeableTypes),
      valuesList(groupedt2Trades.getOrElse("Moved In", Map())),
      valuesList(groupedt2Trades.getOrElse("Moved Out", Map())),
      valuesList(groupedt2Trades.getOrElse("Created", Map())),
      valuesList(deletedTrades),
      valuesList(groupedt2Trades.getOrElse("Undeleted", Map())),
      groupedt2Trades.getOrElse("Amended", Map()).map{case (tradeID, tradeT2) => (t1Trades(tradeID), tradeT2)}.toList
    )
  }

  def reportPivot(
    timestamp: Timestamp,
    marketDay: Day,
    expiryDay: Day,
    predicate: TradePredicate,
    utpPartitioningFields: List[PField],
    addRows : Boolean //Before the user has pressed run we just show the fields
  ) = pivot(timestamp, Some(marketDay), expiryDay, predicate, utpPartitioningFields, addRows)


  def pivot(
    timestamp: Timestamp,
    marketDay : Option[Day],
    expiryDay: Day,
    predicate: TradePredicate
  ) : PivotTableDataSource = pivot(timestamp, marketDay, expiryDay, predicate, Nil, true)

  private val pivotCache = CacheFactory.getCache("TradeStore.pivot")

  def pivot(
             timestamp : Timestamp,
             marketDay : Option[Day],
             expiryDay : Day,
             predicate : TradePredicate,
             utpPartitioningFields : List[PField],
             addRows : Boolean) : PivotTableDataSource = {
    pivotCache.memoize(
      (timestamp, marketDay, expiryDay, predicate, utpPartitioningFields, addRows),
      { createPivot(timestamp, marketDay, expiryDay, predicate, utpPartitioningFields, addRows)}
    )
  }

  private def createPivot(
                           timestamp : Timestamp,
                           marketDay : Option[Day],
                           expiryDay : Day,
                           predicate : TradePredicate,
                           utpPartitioningFields : List[PField],
                           addRows : Boolean) : PivotTableDataSource = {
    updateTradeHistories(timestamp, Some(expiryDay))

    val filter = fromPredicate(predicate)

    new UnfilteredPivotTableDataSource(){
      private val tradeableTypes = new scala.collection.mutable.HashSet[TradeableType[_]]
      val pivotData = {
        val versions = tradeHistories.tradeRowsAsOf(timestamp, Some(expiryDay), marketDay=marketDay)
        println("There are " + versions.size + " versions")
        Log.infoWithTime("Building pivot data") {
          versions.flatMap { case TradeAndFields(_, trade, details) => {
            tradeableTypes += trade.tradeable.tradeableType
            val joinedTradeAttributeDetails = joiningTradeAttributeFieldValues(trade.attributes)
            val tradeFields = new AppendingMap(Map("Join"->joinedTradeAttributeDetails) ++ details.maps)
            if (filter(tradeFields)) {
              val utps = tradeHistories.tradeUTPs(trade)
              utps.map {
                case (utp, volume) => {
                  val utpID = {
                    val id = tradeHistories.utps(utp)
                    val partitions = utpPartitioningFields.map {field => (field.name, joinedTradeAttributeDetails(field).toString)}.toMap
                    new UTPIdentifier(id.asInstanceOf[Int], partitions)
                  }
                  new AppendingMap(Map(
                    "UTP"->Map(
                      PField(instrumentID_str) -> utpID,
                      PField(tradeCount_str) -> details(PField(tradeID_str)),
                      PField(utpVolume_str) -> volume
                    )) ++ tradeFields.maps //++tradeHistories.utpDetails(utp),
                  )
                }

              }
            } else {
              Nil
            }
          }}.toList
        }
      }
      println("There are " + pivotData.size + " rows")

      def unfilteredData(pfs : PivotFieldsState) = { if (addRows) pivotData else List() }

      val fieldDetailsGroups: List[FieldDetailsGroup] = createFieldDetailGroups(tradeableTypes.toSet)
      override val drillDownGroups = pivotDrillDownGroups
      override def initialState = pivotInitialState(tradeableTypes.toSet)
    }

  }

  private def createFieldDetailGroups(tradeableTypes:Set[TradeableType[_]]) = {
    val allFieldNames = Set() ++ tradeableTypes.flatMap(_.fields)
    List(
      FieldDetailsGroup("Trade Fields", JustTradeFields.fieldDetails),
      FieldDetailsGroup("Trade System Fields", tradeAttributeFieldDetails),
      FieldDetailsGroup("Instrument Fields", TradeableFields.fieldDetails.filter(fd=>allFieldNames.contains(fd.field.name)))
    )
  }

  def latestTimestamp() = {
    updateTradeHistories(earliestTimestamp, None)
    cachedLatestTimestamp.get
  }

  def utps(timestamp: Timestamp, marketDay: Day, expiryDay: Day, predicate: TradePredicate, utpPartitioningFields: List[PField]): Map[UTPIdentifier, UTP] = {
    updateTradeHistories(timestamp, Some(expiryDay))
    val filter = fromPredicate(predicate)
    tradeHistories.tradeRowsAsOf(timestamp, Some(expiryDay), marketDay=Some(marketDay)).flatMap{
      case TradeAndFields(_, trade, details) => {

        val joinedTradeAttributeDetails = joiningTradeAttributeFieldValues(trade.attributes)
        val tradeFields = new AppendingMap(Map("Join"->joinedTradeAttributeDetails) ++ details.maps)
        if (filter(tradeFields)) {
          val utps = tradeHistories.tradeUTPs(trade)
          utps.map{
            case (utp, volume) => {
              val utpID = {
                val id = tradeHistories.utps(utp)
                val partitions = utpPartitioningFields.map {field => (field.name, tradeFields(field).toString)}.toMap
                new UTPIdentifier(id.asInstanceOf[Int], partitions)
              }
              (utpID -> utp)
            }
          }
        } else {
          Nil
        }
      }
      case _ => Nil
    }.toMap
  }

  private def tradeFromRow(row: RichInstrumentResultSetRow) = {
    TradeStore.tradeFromRow(row, tradeSystem, createTradeAttributes(row))
  }

  def pivotInitialState(tradeableTypes:Set[TradeableType[_]]): PivotFieldsState

  protected val instrumentFilteredDrillDown = {
    import starling.pivot.{Field => PivotField}
    val fallBack = PivotAxis( TradeableType.drillDownFields.map(f=>PivotField(f)), List(PivotField("Trade ID")), List(), false)
    val instrumentToFields = TradeableType.types.flatMap(inType => {
      inType match {
        case y:TradeableType[_] => {Some((y.name, y.fields.map(PivotField(_))))}
        case n => None
      }
    })
    DrillDownInfo(fallBack, Some(FilteredDrillDown(PivotField("Instrument"), instrumentToFields)))
  }

  def pivotDrillDownGroups(): List[DrillDownInfo]

  /**
   * gets the largest ID of any trade in the named subgroup, or 0 if there are no trades. this assumes that the IDs are
   * strictly increasing, which is probably always true.
   */
  protected def tradesHash(predicate: (Trade) => Boolean): Long = {
    (0 :: readLatestVersionOfAllTrades.filter(v => predicate(v._2.trade)).map(_._2.id).toList).max
  }

  def storeTrades(predicate: (Trade) => Boolean, trades: Iterable[Trade], timestamp: Timestamp): StoreResults = {
    var result: StoreResults = null
    inTransaction(writer => {
      val allTrades = readLatestVersionOfAllTrades
      val currentTrades = allTrades.filter(v => predicate(v._2.trade))
      val (added, updated, _) = writer.updateTrades(trades, currentTrades, timestamp)

      val currentTradeIDs = currentTrades.map(_._1)
      // the update step above doesn't remove trades, so this needs to be done as a separate step
      val updateTradeIDs = Set[TradeID]() ++ trades.map(_.tradeID)
      val deletedTradeIDs = currentTradeIDs.filterNot(id => updateTradeIDs.contains(id)).toSet
      writer.delete(deletedTradeIDs, timestamp)

      Log.info("Deleting " + deletedTradeIDs.size + " trades for " + bookID + ". " + (allTrades.size, currentTrades.size, trades.size, added, updated))

      val hash = tradesHash(predicate)
      result = StoreResults(added, deletedTradeIDs.size, updated, hash)
    })

    assume(result != null, "Something went wrong storing the trades: " + trades) // sanity check
    result
  }

  class Writer(writer: DBWriter) {

    private def findMaxID = {
      val q = (
              select("max(id) i")
                      from (tableName + " t")
              )
      db.queryWithOneResult(q) {row => {if (row.isNull("i")) 0 else row.getInt("i")}}.get
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
          assert(instrument.isInstanceOf[ErrorInstrument] || instrument.isInstanceOf[DeletedInstrument], "Wrong type:" + instrument)
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
      if(toInsertTradeTable.size > 100) {
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
              if(oldTradeRow.trade.toString == updateTrade.toString) {
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
      val latestVersionOfTrades = readLatestVersionOfTrades(tradeIDs)
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
      case e: Throwable => throw new Exception(e.getMessage + " in row " + row.toString, e)
    }
  }
}

