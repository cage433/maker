package starling.trade.impl

import starling.auth.User
import starling.instrument.TradeID._
import starling.gui.api.TradeSystemLabel._
import starling.daterange.{Day, Timestamp}
import starling.pivot.model.PivotTableModel
import starling.utils.cache.CacheFactory
import starling.trades.internal.TradeChangesPivotTableDataSource
import starling.services.trade.TradeDiff._
import starling.tradestore.TradePredicate._
import starling.pivot.Field._
import starling.tradestore.intraday.IntradayTradeAttributes
import starling.pivot.SomeSelection._
import starling.pivot._
import controller.PivotTable
import starling.rmi.PivotData._
import starling.utils.{STable, Log}
import java.util.concurrent.ConcurrentHashMap
import java.util.UUID
import starling.gui.api._
import starling.gui.api.TradeIDLabel._
import starling.instrument.{Trade, TradeSystem, TradeID}
import starling.tradestore.{TradeStores, TradePredicate, TradeSet}
import starling.services.trade.TradeDiff
import starling.rmi.{PivotData, TradeReconciliation, Permission}
import starling.db.{DB, TitanTradeSystem, IntradayTradeSystem, TradeSystems}
import starling.eai.Traders
import starling.trade.facility.TradeFacility

class TradeFacilityImpl(
                        tradeStores:TradeStores,
                        enabledDesks: Set[Desk],
                        val allTraders: Traders,
                        eaiStarlingDB: DB,
                        isProduction:Boolean
                        ) extends TradeFacility with Log {
  def desks = {
    val user = User.currentlyLoggedOn
    val enabled = tradeStores.deskDefinitions.keysIterator.toList.filter(enabledDesks.contains)
    val desksAllowed = Permission.desks(user, isProduction)
    val userDesks = enabled.filter(desksAllowed.contains)
    log.info("Getting desks for user: " + user.name + ", desks: " + userDesks)
    userDesks
  }

  def groupToDesksMap = Permission.groupToDesksMap(isProduction)

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
        List(new TradeSet(IntradayTradeSystem, tradeStores.intradayTradeStore,
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
    val changed = tradeStores.findImporter(TitanTradeSystem).importAll(None, ts)
    if (changed) {
      tradeStores.closedDesks.closeDesk(Desk.Titan, Day.today, ts)
    }
  }

  def bookClose(desk: Desk) {
    val valid = List(Desk.GasolineSpec, Desk.LondonDerivatives).map(_.name)
    val bookID = desk match {
      case Desk(name, _, Some(info:EAIDeskInfo)) if valid.contains(name) => info.book
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

  def deskCloses = tradeStores.closedDesksByDay
  def latestTradeTimestamp(desk:Desk):TradeTimestamp = tradeStores.closedDesks.latestTradeTimestamp(desk)

  def intradayLatest = tradeStores.intradayTradeStore.intradayLatest

  def selectLiveAndErrorTrades(day: Day, timestamp: Timestamp, desk: Desk, tradePredicate: TradePredicate):List[Trade] = {
    deskTradeSets(Some(desk), tradePredicate).flatMap(_.selectLiveAndErrorTrades(day, timestamp))
  }

  def traders = allTraders.deskMap
}