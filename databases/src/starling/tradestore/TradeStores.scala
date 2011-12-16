package starling.tradestore

import eai.EAITradeStore
import intraday.{IntradayTradeAttributes, IntradayTradeStore}
import starling.db._
import starling.neptune.{RefinedFixationTradeStore, RefinedAssignmentTradeStore}
import starling.daterange.{Day, Timestamp}
import starling.instrument.{Trade, TradeID, TradeSystem}
import starling.pivot._
import starling.tradeimport.{ClosedDesks, TradeImporter}
import starling.eai.instrumentreaders.EAISystemOfRecord
import starling.eai.{TreeID}
import starling.utils.STable
import starling.gui.api._
import scala.collection.JavaConversions._
import collection.Iterable
import scalaz.Scalaz._


trait DeskDefinition {
  def tradeSets(predicate: TradePredicate): List[TradeSet]

  def initialState: Option[PivotFieldsState] = None

  def tradeTimestampForOffset(closeDay: Day): TradeTimestamp = throw new Exception("Not implemented for " + this)
}

/**
 * Holds all the trades stores and has methods to retrieve the appropriate one
 */
case class TradeStores(
                        closedDesks: ClosedDesks,
                        eaiTradeStores: Map[Desk, EAITradeStore],
                        intradayTradeStore: IntradayTradeStore,
                        titanTradeStore: RichTradeStore,
                        val enabledDesks: Set[Desk]
                        ) {

  def registerTradeImporter(key: Object, tradeImporter: TradeImporter) {
    tradeImporters.put(key, tradeImporter)
  }

  def unregister(key: Object) {
    tradeImporters.remove(key)
  }

  val tradeImporters = new java.util.concurrent.ConcurrentHashMap[Object, TradeImporter]()

  private def label(fieldDetailsGroup: FieldDetailsGroup): FieldDetailsGroupLabel = FieldDetailsGroupLabel(fieldDetailsGroup.name, fieldDetailsGroup.fields.map(_.field.name))

  def readTradeVersions(tradeID: TradeID): (STable, List[FieldDetailsGroupLabel], List[CostsLabel]) = {
    storesFor(tradeID.tradeSystem).foreach {
      tradeStore => {
        tradeStore.tradeHistory(tradeID) match {
          case Some(res) => return (res._1, res._2.map(label(_)), res._3)
          case None =>
        }
      }
    }
    throw new Exception("Trade " + tradeID + " not found")

  }


  def all = eaiTradeStores.values.toList ::: List(intradayTradeStore, titanTradeStore)

  private def eaiDesk(desk: Desk) = {
    desk -> new DeskDefinition() {
      def tradeSets(predicate: TradePredicate) = List(
        new TradeSet(EAITradeSystem, eaiStoreFor(desk),
          predicate.addFilter(Field("Desk"), Set(desk.name)))
      )

      override def initialState = Some(PivotFieldsState(
        dataFields = List(Field("Trade Count")),
        rowFields = List(Field("Instrument"), Field("Market")),
        filters = List((Field("Strategy"), AllSelection))
      ))

      override def tradeTimestampForOffset(closeDay: Day) = {
        closedDesks.closedDesksByOffset(desk, closeDay)
      }
    }
  }

  def findImporter(tradeSystem: TradeSystem) = {
    tradeImporters.values().find(_.tradeSystem == tradeSystem).getOrElse(throw new Exception("No importer found for " + tradeSystem))
  }

  val deskDefinitions = Map[Desk, DeskDefinition](
    Desk.Titan -> new DeskDefinition() {
      def tradeSets(predicate: TradePredicate) = List(
        new TradeSet(TitanTradeSystem, standardStoreFor(TitanTradeSystem), predicate)
      )
    }
  ) ++ Desk.eaiDesks.map(eaiDesk)

  def eaiStoreFor(desk: Desk) = eaiTradeStores(desk)

  def standardStoreFor(tradeSystemName: TradeSystem): RichTradeStore = tradeSystemName match {
    case IntradayTradeSystem => intradayTradeStore
    case TitanTradeSystem => titanTradeStore
  }

  def storesFor(tradeSystem: TradeSystem): Iterable[RichTradeStore] = {
    tradeSystem match {
      case EAITradeSystem => {
        eaiTradeStores.values
      }
      case _ => List(standardStoreFor(tradeSystem))
    }
  }

  def titanCurrentTimestamp = TradeTimestamp.makeMagicLatestTimestamp(titanTradeStore.latestKnownTimestamp.getOrElse(new Timestamp(0)))

  def closedDesksByDay: Map[Desk, Map[Day, List[TradeTimestamp]]] = {
    val closes = closedDesks.closedDesksByDay
    val titanLatestTradeTimestamp = titanCurrentTimestamp
    val allCloses = closes + (Desk.Titan -> (closes.getOrElse(Desk.Titan, Map()) + (TradeTimestamp.magicLatestTimestampDay -> List(titanLatestTradeTimestamp))))
    Map() ++ allCloses.filterKeys(enabledDesks.contains(_))
  }

  def toTradeSets(tradeSelection: TradeSelectionWithTimestamp): List[(TradeSet, Timestamp)] = {
    val currentGroups = intradayTradeStore.intradayLatest.keySet
    val intradayTradesetsAndTimestamps = tradeSelection.intradaySubgroupAndTimestamp.toList.flatMap {
      case (subgroups, ts) => {

        val subgroupsToUse = subgroups.subgroups.flatMap(subgroup => {
          if (currentGroups.contains(subgroup)) {
            List(subgroup)
          } else {
            currentGroups.filter(_.startsWith(subgroup + "/")).toList
          }
        })
        val predicate = tradeSelection.deskAndTimestamp.toList.flatMap {
          case (desk, tradeTimestamp) => {
            List((Field("Entry Date"), GreaterThanSelection(tradeTimestamp.closeDay)), (Field("Desk"), SomeSelection(Set(desk.name))))
          }
        } ::: List((Field(IntradayTradeAttributes.subgroupName_str), SomeSelection(subgroupsToUse.toSet)))

        List((new TradeSet(IntradayTradeSystem, intradayTradeStore,
          TradePredicate(predicate ::: tradeSelection.tradePredicate.filter, tradeSelection.tradePredicate.selection)),
          ts))
      }
    }

    val deskTradeSetsAndTimestamps = tradeSelection.deskAndTimestamp.toList.flatMap {
      case (desk, ts) => {
        val predicate: List[(Field, Selection)] = tradeSelection.deskAndTimestamp.toList.flatMap {
          case (desk, tradeTimestamp) => {
            List((Field("Trade Day"), LessThanOrEqualSelection(tradeTimestamp.closeDay)))
          }
        }
        deskDefinitions(desk).tradeSets(TradePredicate(predicate ::: tradeSelection.tradePredicate.filter, tradeSelection.tradePredicate.selection)).map((_, ts.timestamp))
      }
    }

    intradayTradesetsAndTimestamps ::: deskTradeSetsAndTimestamps
  }
}

class TradeSet(
                val tradeSystem: TradeSystem,
                val tradeStore: RichTradeStore,
                val tradePredicate: TradePredicate) {
  val key = (tradeSystem, tradePredicate)

  def partitioningTradeColumns: List[Field] = {
    if (tradeSystem == EAITradeSystem) List(Field("Strategy")) else Nil
  }

  def tradeIDFor(text: String) = {
    val possibleID = TradeID(text, tradeSystem)
    tradeStore.readTrade(possibleID, None) match {
      case Some(trade) => Some(possibleID)
      case None => None
    }
  }

  def reportPivot(marketDay: Day, expiryDay: Day, timestamp: Timestamp, addRows: Boolean) =
    tradeStore.reportPivot(timestamp, marketDay, expiryDay, tradePredicate, partitioningTradeColumns, addRows)

  def utps(marketDay: Day, expiryDay: Day, timestamp: Timestamp) = {
    tradeStore.utps(timestamp, marketDay, expiryDay, tradePredicate, partitioningTradeColumns)
  }

  def tradeChanges(t1: Timestamp, t2: Timestamp, expiryDay: Day): TradeChanges = {
    assert(t1 <= t2, "t1 should be on or before t2: " +(t1, t2))
    tradeStore.tradeChanges(t1, t2, expiryDay, tradePredicate)
  }

  def readAll(t: Timestamp, expiryDay: Option[Day] = None, marketDay: Option[Day] = None) = tradeStore.readAll(t, tradePredicate, expiryDay, marketDay)

  def pivot(expiryDay: Day, timestamp: Timestamp) = {
    tradeStore.pivot(timestamp, None, expiryDay, tradePredicate)
  }

  def forTradeDays(tradeDays: Set[Day]) = {
    new TradeSet(tradeSystem, tradeStore, TradePredicate((Field("Trade Day"), new SomeSelection(tradeDays.asInstanceOf[Set[Any]])) :: tradePredicate.filter, tradePredicate.selection))
  }
}

