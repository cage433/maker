package starling.tradestore

import eai.EAITradeStore
import intraday.{IntradayTradeAttributes, IntradayTradeStore}
import starling.db._
import starling.neptune.{RefinedFixationTradeStore, RefinedAssignmentTradeStore}
import starling.daterange.{Day, Timestamp}
import starling.trade.{Trade, TradeID, TradeSystem}
import starling.pivot._
import starling.gui.api.{TradeTimestamp, Desk, TradeSelectionWithTimestamp}
import starling.tradeimport.{ClosedDesks, TradeImporter}
import starling.eai.instrumentreaders.EAISystemOfRecord
import starling.eai.{Book, TreeID}

trait DeskDefinition {
  def tradeSets(predicate:TradePredicate):List[TradeSet]
  def initialState:Option[PivotFieldsState] = None
  def tradeTimestampForOffset(closeDay: Day): TradeTimestamp = throw new Exception("Not implemented for " + this)
}

/**
 * Holds all the trades stores and has methods to retrieve the appropriate one
 */
case class TradeStores(
  tradeImporters:Map[TradeSystem,TradeImporter],
  closedDesks: ClosedDesks,
  eaiTradeStores: Map[Book,EAITradeStore],
  intradayTradeStore: IntradayTradeStore,
  refinedAssignmentTradeStore: RefinedAssignmentTradeStore,
  refinedFixationTradeStore : RefinedFixationTradeStore
) {

  def all = eaiTradeStores.values.toList ::: List(intradayTradeStore, refinedAssignmentTradeStore, refinedFixationTradeStore)

  val deskDefinitions = Map[Desk,DeskDefinition](
    Desk.LondonDerivativesOptions -> new DeskDefinition() {
      def tradeSets(predicate: TradePredicate) = List(
        new TradeSet(EAITradeSystem, eaiStoreFor(Book.LondonDerivativesOptions), tradeImporters.get(EAITradeSystem),
          predicate.addFilter(Field("Book"), Set(TreeID(EAISystemOfRecord.LONDON_DERIVS_OPTIONS_BOOK))))
      )
      override def initialState = Some(PivotFieldsState(
        dataFields=List( Field("Trade Count") ),
        rowFields=List( Field("Instrument"), Field("Market")),
        filters=List( (Field("Strategy"), new SomeSelection(Set(PivotTreePath("Strategies/Spec/London/London Derivatives Options/Options AM/JF Deals")))))
      ))

      override def tradeTimestampForOffset(closeDay: Day) = {
        closedDesks.closedDesksByOffset(Desk.LondonDerivativesOptions, closeDay)
      }
    },
    Desk.GasolineSpec -> new DeskDefinition() {
      def tradeSets(predicate: TradePredicate) = List(
        new TradeSet(EAITradeSystem, eaiStoreFor(Book.GasolineSpec), tradeImporters.get(EAITradeSystem),
          predicate.addFilter(Field("Book"), Set(TreeID(EAISystemOfRecord.GasolineSpec))))
      )
      override def initialState = Some(PivotFieldsState(
        dataFields=List( Field("Trade Count") ),
        rowFields=List( Field("Instrument"), Field("Market")),
        filters=List( (Field("Strategy"), new SomeSelection(Set(PivotTreePath("Strategies/Spec/London/Seetal")))))
      ))

      override def tradeTimestampForOffset(closeDay: Day) = {
        closedDesks.closedDesksByOffset(Desk.GasolineSpec, closeDay)
      }
    },
    Desk.LondonDerivatives -> new DeskDefinition() {
      def tradeSets(predicate: TradePredicate) = List(
        new TradeSet(EAITradeSystem, eaiStoreFor(Book.LondonDerivatives), tradeImporters.get(EAITradeSystem),
          predicate.addFilter(Field("Book"), Set(TreeID(EAISystemOfRecord.LONDON_DERIVS_BOOK))))
      )
      override def initialState = Some(PivotFieldsState(
        dataFields=List( Field("Trade Count") ),
        rowFields=List( Field("Instrument"), Field("Market")),
        filters=List( (Field("Strategy"), AllSelection))
      ))

      override def tradeTimestampForOffset(closeDay: Day) = {
        closedDesks.closedDesksByOffset(Desk.LondonDerivatives, closeDay)
      }
    },
    Desk.CrudeSpecNorthSea -> new DeskDefinition() {
      def tradeSets(predicate: TradePredicate) = List(
        new TradeSet(EAITradeSystem, eaiStoreFor(Book.CrudeSpecNorthSea), tradeImporters.get(EAITradeSystem),
          predicate.addFilter(Field("Book"), Set(TreeID(EAISystemOfRecord.CrudeSpecNorthSea))))
      )
      override def initialState = Some(PivotFieldsState(
        dataFields=List( Field("Trade Count") ),
        rowFields=List( Field("Instrument"), Field("Market")),
        filters=List( (Field("Strategy"), AllSelection))
      ))

      override def tradeTimestampForOffset(closeDay: Day) = {
        closedDesks.closedDesksByOffset(Desk.CrudeSpecNorthSea, closeDay)
      }
    },
    Desk.HoustonDerivatives -> new DeskDefinition() {
      def tradeSets(predicate: TradePredicate) = List(
        new TradeSet(EAITradeSystem, eaiStoreFor(Book.HoustonDerivatives), tradeImporters.get(EAITradeSystem),
          predicate.addFilter(Field("Book"), Set(TreeID(EAISystemOfRecord.HoustonDerivatives))))
      )
      override def initialState = Some(PivotFieldsState(
        dataFields=List( Field("Trade Count") ),
        rowFields=List( Field("Instrument"), Field("Market")),
        filters=List( (Field("Strategy"), AllSelection))
      ))

      override def tradeTimestampForOffset(closeDay: Day) = {
        closedDesks.closedDesksByOffset(Desk.HoustonDerivatives, closeDay)
      }
    },
    Desk.Refined -> new DeskDefinition() {
      def tradeSets(predicate: TradePredicate) = List(
        new TradeSet(RefinedAssignmentTradeSystem, standardStoreFor(RefinedAssignmentTradeSystem), tradeImporters.get(RefinedAssignmentTradeSystem), predicate),
        new TradeSet(RefinedFixationTradeSystem, standardStoreFor(RefinedFixationTradeSystem), tradeImporters.get(RefinedFixationTradeSystem), predicate)
      )
    }

  )

  def eaiStoreFor(book:Book) = eaiTradeStores(book)

  def standardStoreFor(tradeSystemName: TradeSystem):TradeStore = tradeSystemName match {
    case IntradayTradeSystem => intradayTradeStore
    case RefinedAssignmentTradeSystem  => refinedAssignmentTradeStore
    case RefinedFixationTradeSystem => refinedFixationTradeStore
  }

  def storesFor(tradeSystem:TradeSystem) = {
    tradeSystem match {
      case EAITradeSystem => {
        val all = eaiTradeStores.values
        val (loaded, notLoaded) = all.partition(_.isLoaded)
        loaded.toList ::: notLoaded.toList
      }
      case _ => List( standardStoreFor(tradeSystem) )
    }
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
        val predicate = tradeSelection.deskAndTimestamp.toList.map {
          case (desk, tradeTimestamp) => (Field("Entry Date"), GreaterThanSelection(tradeTimestamp.closeDay))
        } ::: List((Field(IntradayTradeAttributes.subgroupName_str), SomeSelection(subgroupsToUse.toSet)))

        List((new TradeSet(IntradayTradeSystem, intradayTradeStore, None,
          TradePredicate(predicate ::: tradeSelection.tradePredicate.filter, tradeSelection.tradePredicate.selection)),
                ts))
      }
    }

    val deskTradeSetsAndTimestamps = tradeSelection.deskAndTimestamp.toList.flatMap {
      case (desk, ts) => deskDefinitions(desk).tradeSets(tradeSelection.tradePredicate).map((_, ts.timestamp))
    }

    intradayTradesetsAndTimestamps ::: deskTradeSetsAndTimestamps
  }
}

class TradeSet(
        val tradeSystem:TradeSystem,
        tradeStore:TradeStore,
        tradeImporter:Option[TradeImporter],
        val tradePredicate:TradePredicate) {
  val key = (tradeSystem, tradePredicate)
  def importAll = tradeImporter match {
    case Some(importer) => Some(importer.importAll())
    case None => None
  }

  def partitioningTradeColumns : List[Field] = {
    if (tradeSystem == EAITradeSystem) List(Field("Strategy")) else Nil
  }


  def selectLiveAndErrorTrades(marketDay: Day, timestamp: Timestamp): List[Trade] = {
    tradeStore.selectLiveAndErrorTrades(tradePredicate, marketDay, timestamp)
  }

  def tradeIDFor(text: String) = {
    val possibleID = TradeID(text, tradeSystem)
    tradeStore.readTrade(possibleID, None) match {
      case Some(trade) => Some(possibleID)
      case None => None
    }
  }

  def reportPivot(marketDay: Day, expiryDay: Day, timestamp: Timestamp, addRows:Boolean) =
    tradeStore.reportPivot(timestamp, marketDay, expiryDay, tradePredicate, partitioningTradeColumns, addRows)

  def utps(marketDay: Day, expiryDay: Day, timestamp: Timestamp) = {
    tradeStore.utps(timestamp, marketDay, expiryDay, tradePredicate, partitioningTradeColumns)
  }

  def tradeChanges(t1: Timestamp, t2: Timestamp, expiryDay: Day) : TradeChanges = {
    assert(t1 <= t2, "t1 should be on or before t2: " + (t1, t2))
    tradeStore.tradeChanges(t1, t2, expiryDay, tradePredicate)
  }

  def readAll(t:Timestamp, expiryDay: Option[Day] = None) = tradeStore.readAll(t, tradePredicate, expiryDay)

  def pivot(expiryDay: Day, timestamp: Timestamp) = {
    tradeStore.pivot(timestamp, None, expiryDay, tradePredicate)
  }

  def forTradeDays(tradeDays: Set[Day]) = {
    new TradeSet(tradeSystem, tradeStore, tradeImporter, TradePredicate((Field("Trade Day"), new SomeSelection(tradeDays.asInstanceOf[Set[Any]])) :: tradePredicate.filter, tradePredicate.selection))
  }
}

