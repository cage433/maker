package starling.services

import starling.gui.api._
import starling.db.MarketDataStore
import starling.utils.ImplicitConversions._
import starling.titan.EDMConversions
import starling.marketdata._
import starling.quantity.{Quantity, UOM}
import collection.immutable.Map
import starling.market.FuturesExchange
import starling.daterange.Day
import scalaz._
import Scalaz._

case class DataFlow(exchange: FuturesExchange, // is this valid for all data flows ?
                    pricingGroup: PricingGroup, markets: List[String],
                    from: String, to: String, source: String, description: String = "unspecified description") {
  val sink = pricingGroup.name + '.' + exchange.name
}


trait MarketDataEventSource {
  type Key
  type MarketType
  type CurveType

  def marketDataProvider: DataFlowDataProvider[Key, MarketType, CurveType]
  def matches(selection: MarketDataSelection): Boolean
  def eventsFor(previousSnapshot: Option[SnapshotIDLabel], newSnapshot: SnapshotIDLabel, observationDays: List[Day]): List[MarketDataEvent]
  def marketDataEvent(value: Key, marketTypes: List[MarketType], snapshot: SnapshotIDLabel, isCorrection: Boolean): MarketDataEvent
}

trait NullMarketDataEventSource extends MarketDataEventSource {
  type Key = Nothing
  type MarketType = Nothing
  type CurveType = Nothing

  def marketDataProvider: DataFlowDataProvider[Key, MarketType, CurveType] = null
  def matches(selection: MarketDataSelection) = false
  def eventsFor(previousSnapshot: Option[SnapshotIDLabel], newSnapshot: SnapshotIDLabel, observationDays: List[Day]) = Nil
  def marketDataEvent(value: Key, marketTypes: List[MarketType], snapshot: SnapshotIDLabel, isCorrection: Boolean) = null
}

abstract class PricingGroupMarketDataEventSource extends MarketDataEventSource {
  val pricingGroup: PricingGroup
  def matches(selection: MarketDataSelection) = selection.pricingGroup == pricingGroup

  def eventsFor(previousSnapshot: Option[SnapshotIDLabel], newSnapshot: SnapshotIDLabel, observationDays: List[Day]) = {
    val previousCurves: NestedMap[Key, MarketType, CurveType] =
      previousSnapshot.map(marketDataProvider.marketDataFor(pricingGroup, _, observationDays)).getOrElse(Map.empty)
    val curves: NestedMap[Key, MarketType, CurveType] = marketDataProvider.marketDataFor(pricingGroup, newSnapshot, observationDays)

    val events: List[MarketDataEvent] = curves.flatMap { case (key, curveByMarketType) => {
      val groupedRates : NestedMap[String, MarketType, CurveType]= curveByMarketType.groupBy{
        case (marketType, curve) => previousCurves.getOrElse(key, Map[MarketType, CurveType]()).get(marketType) match {
          case None => "New Curve"
          case Some(curve2) if curve == curve2 => "Unchanged"
          case _ => "Correction"
        }
      }

      groupedRates.get("New Curve").map{newCurves => marketDataEvent(key, newCurves.keySet.toList, newSnapshot, isCorrection = false)}.toList :::
      groupedRates.get("Correction").map{newCurves => marketDataEvent(key, newCurves.keySet.toList, newSnapshot, isCorrection = true)}.toList
    }}.toList

    events
  }
}

case class SpotFXDataEventSource(pricingGroup: PricingGroup, marketDataProvider: DataFlowDataProvider[Day, UOM, Quantity])
  extends PricingGroupMarketDataEventSource {

  type Key = Day
  type MarketType = UOM
  type CurveType = Quantity
  def marketDataEvent(observationDay: Day, currencies: List[UOM], snapshot: SnapshotIDLabel, isCorrection: Boolean) =
    SpotFXDataEvent(observationDay, currencies, snapshot, isCorrection)
}


trait DataFlowDataProvider[K, MarketType, CurveType]{
  def marketDataFor(pricingGroup : PricingGroup, label: SnapshotIDLabel, observationDays: List[Day]): NestedMap[K, MarketType, CurveType]
}

case class SpotFXDataProvider (marketDataStore : MarketDataStore) extends DataFlowDataProvider[Day, UOM, Quantity] {
  private val titanCurrencies = EDMConversions.starlingUomSymbolToEdmUom.mapKeys(UOM.asUOM)
    .filterKeys(uom => uom.isCurrency && uom != UOM.USD)
  private val marketDataKeys: Some[Set[MarketDataKey]] = Some(titanCurrencies.keySet.map(SpotFXDataKey(_)))


  def marketDataFor(pricingGroup: PricingGroup, label: SnapshotIDLabel, observationDays: List[Day]) = {
    val selection = MarketDataSelection(Some(pricingGroup))

    val data = marketDataStore.query(MarketDataIdentifier(selection, SnapshotMarketDataVersion(label)), SpotFXDataType.name,
      observationDays = Some(observationDays.map(some(_)).toSet),
      marketDataKeys = marketDataKeys).map { case (timedKey, data) =>

      (timedKey.day, timedKey.key.asInstanceOf[SpotFXDataKey].ccy, data.asInstanceOf[SpotFXData].rate)
    }

    data.collect {
      case (Some(observationDay), currency, rate) => (observationDay, (currency, rate))
    }.groupInto(_._1, _._2).mapValues(_.toMap)
  }
}


case class ReferenceInterestDataProvider (marketDataStore : MarketDataStore) extends DataFlowDataProvider[(Day, String), UOM, PriceFixingsHistoryData]{
  def marketDataFor(pricingGroup : PricingGroup, label: SnapshotIDLabel, observationDays: List[Day]) = {

    val identifier = MarketDataIdentifier(MarketDataSelection(Some(pricingGroup)), SnapshotMarketDataVersion(label))

    val fixings: List[(Option[Day], Option[String], String, PriceFixingsHistoryData)] =
      marketDataStore.query(identifier, PriceFixingsHistoryDataType.name, observationDays = Some(observationDays.map(some(_)).toSet))
        .map { case (timedKey, data) => {
          val key = timedKey.key.asInstanceOf[PriceFixingsHistoryDataKey]
          val fixingsForKey = data.asInstanceOf[PriceFixingsHistoryData]

          (timedKey.day, key.exchangeName, key.marketName, fixingsForKey)
        } }

    // (LIBOR, EUR, Map((Level.Close, ON) → Quantity(0.123)))
    val fixingsByDayAndExchange: List[((Day, String), (UOM, PriceFixingsHistoryData))] = fixings.collect {
      case (Some(observationDay), Some(exchangeName), UOM.Currency(currency), fixingsForDayAndExchange) =>
        ((observationDay, exchangeName), (currency, fixingsForDayAndExchange))
    }

    fixingsByDayAndExchange.groupBy(_._1).mapValues(_.map(_._2).toMap)
  }
}


case class PriceFixingDataEventSource(
  pricingGroup: PricingGroup,
  marketDataProvider : DataFlowDataProvider[(Day, String), UOM, PriceFixingsHistoryData]) extends PricingGroupMarketDataEventSource {
  type Key = (Day, String)
  type MarketType = UOM
  type CurveType = PriceFixingsHistoryData


  def marketDataEvent(observationDayWithSource : (Day, String), currencies: List[UOM], snapshot: SnapshotIDLabel, isCorrection: Boolean) = {
    ReferenceInterestRateDataEvent(observationDayWithSource._1, observationDayWithSource._2, currencies, snapshot, isCorrection)
  }
}