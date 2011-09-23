package starling.rmi

import collection.immutable.{Set, List}
import java.util.UUID
import java.util.concurrent.ConcurrentHashMap

import starling.auth.{LdapUserLookup, User}
import starling.calendar.BusinessCalendarSet
import starling.daterange._
import starling.db._
import starling.eai.{Traders}
import starling.gui.api._
import starling.pivot._
import controller.PivotTable
import starling.pivot.model._
import starling.services._
import starling.instrument.{Trade, TradeID, TradeSystem}
import starling.tradestore.{TradeSet, TradePredicate, TradeStores}
import starling.tradestore.intraday.IntradayTradeAttributes
import starling.utils._
import cache.CacheFactory
import starling.dbx.{FalseClause, From, RealTable}

import starling.dbx.QueryBuilder._
import starling.browser.service.Version
import trade.TradeDiff
import starling.trades.internal.TradeChangesPivotTableDataSource

class StarlingServerImpl(
        val name:String,
        userSettingsDatabase:UserSettingsDatabase,
        tradeStores:TradeStores,
        enabledDesks: Set[Desk],
        versionInfo:Version,
        referenceData:ReferenceData,
        ukHolidayCalendar: BusinessCalendarSet,
        ldapSearch: LdapUserLookup,
        eaiStarlingDB: DB,
        val allTraders: Traders,
        rabbitEventDatabase:RabbitEventDatabase
      ) extends StarlingServer with Log {

  def desks = {
    val user = User.currentlyLoggedOn
    val enabled = tradeStores.deskDefinitions.keysIterator.toList.filter(enabledDesks.contains)
    val desksAllowed = Permission.desks(user, version.production)
    val userDesks = enabled.filter(desksAllowed.contains)
    log.info("Getting desks for user: " + user.name + ", desks: " + userDesks)
    userDesks
  }

  def groupToDesksMap = Permission.groupToDesksMap(version.production)

  private def unLabel(tradeID:TradeIDLabel):TradeID = TradeID(tradeID.id, unLabel(tradeID.tradeSystem))
  private def unLabel(tradeSystem:TradeSystemLabel):TradeSystem = TradeSystems.fromName(tradeSystem.name)

  private def label(tradeSystem:TradeSystem) = TradeSystemLabel(tradeSystem.name, tradeSystem.shortCode)

  def tradeChanges(tradeSelection:TradeSelection, from:Timestamp, to:Timestamp, expiryDay:Day, pivotFieldParams:PivotFieldParams) = {
    val tradeSets = toIntraddayTradeSets(tradeSelection, None) ::: deskTradeSets(tradeSelection.desk, tradeSelection.tradePredicate)
    val pivots = tradeSets.map { tradeSet =>
      tradeChanges(tradeSet, from, to, expiryDay:Day)
    }
    PivotTableModel.createPivotData(UnionPivotTableDataSource.join(pivots), pivotFieldParams)
  }

  val tradeChangesCache = CacheFactory.getCache("PivotReport.tradeChanges", unique = true)

  private def tradeChanges(tradeSet: TradeSet, t1: Timestamp, t2: Timestamp, expiryDay:Day) = {
    val key = List("tradeChanges", tradeSet.key, t1, t2, expiryDay)
    tradeChangesCache.memoize((key), {
      val tradeChanges = Log.infoWithTime("Trade Changes read") {tradeSet.tradeChanges(t1, t2, expiryDay)}
      new TradeChangesPivotTableDataSource(tradeChanges)
    })
  }


  def tradeReconciliation(tradeSelection:TradeSelection, from:TradeTimestamp, to:TradeTimestamp, intradayTimestamp: Timestamp, pivotFieldParams:PivotFieldParams) = {
    val eaiTrades = deskTradeSets(tradeSelection.desk, tradeSelection.tradePredicate)
    assert(eaiTrades.size == 1, "Must specifiy exactly 1 desk selection: " + eaiTrades)
    // we want to reconcile against (start day + 1) up to end day (inclusive). Quite often this will just be one day.
    val entryDays = (from.closeDay upto to.closeDay).toList.filterNot(_ == from.closeDay)
    val intradayTrades = toIntraddayTradeSets(tradeSelection, Some(entryDays))
    def tradeReconciliation(tradeSet1: TradeSet, from: Timestamp, to: Timestamp, intradayTimestamp: Timestamp, tradeSet2: List[TradeSet]) = {
      new TradeReconciliation(TradeDiff(tradeSet1, from, to, intradayTimestamp, tradeSet2))
    }
    val pivot = tradeReconciliation(eaiTrades.head, from.timestamp, to.timestamp, intradayTimestamp, intradayTrades)
    PivotTableModel.createPivotData(pivot, pivotFieldParams)
  }

  /**
   * Desk to TradeSet with no filters.
   */
  private def deskTradeSets(desk:Desk):List[TradeSet] = deskTradeSets(Some(desk), TradePredicate(List(), List()))
  private def deskTradeSets(desk:Option[Desk], tradePredicate:TradePredicate):List[TradeSet] = {
    desk.toList.flatMap(desk=>tradeStores.deskDefinitions(desk).tradeSets(tradePredicate))
  }

  /**
   * return a tradeset for an intraday trade selection, including the trade predicate.
   * If entrayDays is specified then amend the predicate to only match trades with the same entry days.
   */
  private def toIntraddayTradeSets(tradeSelection:TradeSelection, entryDays: Option[List[Day]]):List[TradeSet] = {
    val currentGroups = tradeStores.intradayTradeStore.intradayLatest.keySet
    tradeSelection.intradaySubgroup.toList.flatMap {
      subgroups => {
        val subgroupsToUse = subgroups.subgroups.flatMap(subgroup => {
          if (currentGroups.contains(subgroup)) {
            List(subgroup)
          } else {
            currentGroups.filter(_.startsWith(subgroup + "/")).toList
          }
        })
        val predicate = (Field(IntradayTradeAttributes.subgroupName_str), SomeSelection(Set(subgroupsToUse.toSet))) ::
                entryDays.toList.map(d => (Field("Entry Date"), new SomeSelection(d.toSet)))
        List(new TradeSet(IntradayTradeSystem, tradeStores.intradayTradeStore, None,
          TradePredicate(predicate ::: tradeSelection.tradePredicate.filter, tradeSelection.tradePredicate.selection)))
      }
    }.toList
  }

  def tradePivot(tradeSelection: TradeSelectionWithTimestamp, expiryDay:Day, pivotFieldParams:PivotFieldParams) = {
    val pivots = tradeStores.toTradeSets(tradeSelection).map { case (tradeSet, ts) => tradeSet.pivot(expiryDay, ts) }.toList
    val pivot = pivots.size match {
      case 0 => NullPivotTableDataSource
      case _ => UnionPivotTableDataSource.join(pivots)
    }
    val pivotFieldState = pivotFieldParams.pivotFieldState
    def initialPivotState = {
      tradeSelection.deskAndTimestamp match {
        case Some((desk, timestamp)) => {
          tradeStores.deskDefinitions(desk).initialState match {
            case Some(deskDefault) if (validFieldsState(deskDefault)) => deskDefault
            case _ => pivot.initialState
          }
        }
        case None => pivot.initialState
      }
    }
    def validFieldsState(fieldsState:PivotFieldsState) = {
      val fields = Set() ++ pivot.fieldDetails.map(_.field)
      fieldsState.allFieldsUsed.forall(fields.contains(_))
    }
    val fs = pivotFieldState match {
      case Some(f) => {
        if (validFieldsState(f)) {
          f
        } else {
          initialPivotState
        }
      }
      case None => {
        initialPivotState
      }
    }
    val pivotTable = if (pivotFieldParams.calculate) {
      PivotTableModel.createPivotTableData(pivot, fs)
    } else {
      PivotTable.singleCellPivotTable("Calculation is off")
    }
    val fieldGroups = pivot.fieldDetailsGroups.map(_.toFieldGroup)

    val reportSpecificOptions = pivot.reportSpecificOptions
    val fsToUse = PivotTableModel.setDefaultReportSpecificChoices(reportSpecificOptions, fs)

    val pivotData = PivotData(
      pivot.fieldDetails.map(f=>f.field).toList,
      fieldGroups, Set() ++ pivot.fieldDetails.filter(_.isDataField).map(_.field),
      fsToUse,
      pivot.drillDownGroups,
      pivotTable,
      pivot.availablePages,
      reportSpecificOptions)
    tradeSelection.deskAndTimestamp match {
      case Some((d,t)) if t.error != None => {
        val pt = PivotTable.singleCellPivotTable("Book close error")
        pivotData.copy(pivotTable = pt)
      }
      case _ => pivotData
    }
  }

  def readTradeVersions(tradeIDLabel:TradeIDLabel):(STable,List[FieldDetailsGroupLabel],List[CostsLabel]) = {
    tradeStores.readTradeVersions(unLabel(tradeIDLabel))
  }

  private val importTradesMap = new ConcurrentHashMap[Desk,Boolean]

  def importTitanTrades() {
    val ts = Timestamp.now
    val changed = tradeStores.tradeImporters(TitanTradeSystem).importAll(None, ts)
    if (changed) {
      tradeStores.closedDesks.closeDesk(Desk.Titan, Day.today, ts)
    }
  }

  def bookClose(desk: Desk) {
    val bookID = desk match {
      case Desk(name, _, Some(info:EAIDeskInfo)) => info.book
      case _ => throw new Exception("Book close is not enabled for " + desk)
    }
    val uuid = UUID.randomUUID.toString
    try {
      eaiStarlingDB.inTransaction {
        writer => {
          writer.update("{call spArchiveTrades(" + bookID + ", '" + uuid + "')}")
        }
      }
    } catch {
      case e => {
        log.error("Error doing book close", e)
      }
    }
  }

  def tradeImportText(tradeSelection:TradeSelection) = {
    tradeSelection.desk match {
      case Some(d) => {
        if (!importTradesMap.contains(d)) {
          ("","")
        } else {
          if (importTradesMap.get(d)) {
            ("Imported", "")
          } else {
            ("No Changes", "")
          }
        }
      }
      case None => ("","")
    }
  }

  def tradeIDFor(desk:Desk, text:String):TradeIDLabel = {
    val tradeText = text.trim.toUpperCase
    deskTradeSets(desk).flatMap( tradeSet => tradeSet.tradeIDFor(tradeText).toList) match {
      case Nil => {
        if (text.isEmpty) {
          throw new UnrecognisedTradeIDException("Please enter a trade id")
        } else {
          throw new UnrecognisedTradeIDException("No trade found for " + text)
        }
      }
      case id::Nil => TradeIDLabel(id.id, label(id.tradeSystem))
      case ids => throw new UnrecognisedTradeIDException("Ambigious trade id " + ids)
    }
  }
  def version = versionInfo

  def deskCloses = tradeStores.closedDesksByDay
  def latestTradeTimestamp(desk:Desk):TradeTimestamp = tradeStores.closedDesks.latestTradeTimestamp(desk)

  def intradayLatest = tradeStores.intradayTradeStore.intradayLatest
  def clearCache {}

  def selectLiveAndErrorTrades(day: Day, timestamp: Timestamp, desk: Desk, tradePredicate: TradePredicate):List[Trade] = {
    deskTradeSets(Some(desk), tradePredicate).flatMap(_.selectLiveAndErrorTrades(day, timestamp))
  }

  def referenceDataTables() = referenceData.referenceDataTables()
  def referencePivot(table: ReferenceDataLabel, pivotFieldParams: PivotFieldParams) = referenceData.referencePivot(table, pivotFieldParams)
  def ukBusinessCalendar = ukHolidayCalendar
  def whoAmI = User.currentlyLoggedOn
  def allUserNames:List[String] = userSettingsDatabase.allUsersWithSettings
  def isStarlingDeveloper = {
    if (version.production) {
      User.currentlyLoggedOn.groups.contains(Groups.StarlingDevelopers)
    } else {
      val groups = User.currentlyLoggedOn.groups
      groups.contains(Groups.StarlingDevelopers) || groups.contains(Groups.StarlingTesters)
    }
  }

  def traders = allTraders.deskMap

  def orgPivot(pivotFieldParams:PivotFieldParams) = {
    PivotTableModel.createPivotData(new OrgPivotTableDataSource, pivotFieldParams)
  }

  def userStatsPivot(pivotFieldParams:PivotFieldParams):PivotData = {
    val table = "PageViewLog"
    val pageText = "Page Text"
    val name = "Name"
    val timestamp = "Timestamp"
    val time = "time"
    val dayName = "Day"
    val countName = "Count"
    val columns = {
      List(("Stat Fields", List(
        StringColumnDefinition("User Name", "loginName", table),
        StringColumnDefinition(name, "userName", table),
        StringColumnDefinition(pageText, "text", table),
        StringColumnDefinition("Short Page Text", "shortText", table),
        StringColumnDefinition("Page toString", "pageString", table),
        TimestampColumnDefinition(timestamp, time, table),
        new FieldBasedColumnDefinition(dayName, time, table) {
          def read(resultSet:ResultSetRow) = {
            Day.fromMillis(resultSet.getTimestamp(alias).instant)
          }
          override def filterClauses(values:Set[Any]) = {
            values.asInstanceOf[Set[Day]].map(d => ("YEAR(time)" eql d.year) and ("MONTH(time)" eql d.month) and ("DAY(time)" eql d.dayNumber)).toList
          }
        },
        new FieldBasedColumnDefinition("Month", time, table) {
          def read(resultSet:ResultSetRow) = {
            val day = Day.fromMillis(resultSet.getTimestamp(alias).instant)
            Month(day.year, day.month)
          }
          override def filterClauses(values:Set[Any]) = {
            values.asInstanceOf[Set[Month]].map(m => ("YEAR(time)" eql m.y) and ("MONTH(time)" eql m.m)).toList
          }
        },
        new ColumnDefinition(countName) {
          val alias = "t_count"
          def read(resultSet:ResultSetRow) = resultSet.getInt(alias)
          def filterClauses(values:Set[Any]) = List(FalseClause) //not supported
          override def fieldDetails = new SumIntFieldDetails(name)
          def selectFields = List("count(*) " + alias)
          def orderByFields = List()
          def groupByFields = List()
        }
        )))}

    val dayField = Field(dayName)
    val latestDays: Set[Any] = (Day.today - 2 upto Day.today).toList.toSet

    PivotTableModel.createPivotData(new OnTheFlySQLPivotTableDataSource(
      userSettingsDatabase.db,
      columns,
      From(RealTable(table), List()),
      List(),
      PivotFieldsState(rowFields = List(dayField, Field(name), Field(pageText)), dataFields = List(Field(countName)), filters = (dayField, SomeSelection(latestDays)) :: Nil ),
      List()
    ), pivotFieldParams)
  }

  def storeSystemInfo(info:OSInfo) {userSettingsDatabase.storeSystemInfo(User.currentlyLoggedOn, info)}

  def gitLog(pivotFieldParams:PivotFieldParams, numCommits:Int) = {
    val gitPivotSource = new GitPivotDataSource(numCommits)
    PivotTableModel.createPivotData(gitPivotSource, pivotFieldParams)
  }
}
