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


trait MarketDataEventSource { source =>
  type Key
  type MarketType
  type CurveType
  case class MarketDataChange(observationDay:Day, value: Key, marketTypes: List[MarketType], isCorrection: Boolean) {
    def marketDataEvent(snapshot:SnapshotIDLabel): MarketDataEvent = source.marketDataEvent(this, value, marketTypes, snapshot)
  }
  def marketDataProvider: DataFlowDataProvider[Key, MarketType, CurveType]
  def matches(pricingGroup: PricingGroup): Boolean
  def changesFor(previousVersion: Int, newVersion: Int, observationDays: List[Day]): List[MarketDataChange]
  def marketDataEvent(change:MarketDataChange, key:Key, marketTypes: List[MarketType], snapshot:SnapshotIDLabel): MarketDataEvent
}

trait NullMarketDataEventSource extends MarketDataEventSource {
  type Key = Nothing
  type MarketType = Nothing
  type CurveType = Nothing

  def marketDataProvider: DataFlowDataProvider[Key, MarketType, CurveType] = null
  def matches(pricingGroup:PricingGroup) = false
  def changesFor(previousVersion: Int, newVersion: Int, observationDays: List[Day]): List[MarketDataChange] = Nil
  def marketDataEvent(change:MarketDataChange, key:Nothing, marketTypes: List[Nothing], snapshot:SnapshotIDLabel): MarketDataEvent = null
}


abstract class PricingGroupMarketDataEventSource extends MarketDataEventSource {
  val pricingGroup: PricingGroup
  def matches(pg: PricingGroup) = (pg == pricingGroup)

  def changesFor(previousVersion: Int, newVersion: Int, observationDays: List[Day]) = {
    val changes = observationDays.flatMap { day =>
      val previousCurves: NestedMap[Key, MarketType, CurveType] =
        marketDataProvider.marketDataFor(pricingGroup, previousVersion, day)
      val curves: NestedMap[Key, MarketType, CurveType] = marketDataProvider.marketDataFor(pricingGroup, newVersion, day)

      curves.flatMap { case (key, curveByMarketType) => {
        val groupedRates = curveByMarketType.groupBy{
          case (marketType, curve) => previousCurves.getOrElse(key, Map[MarketType, CurveType]()).get(marketType) match {
            case None => "New Curve"
            case Some(curve2) if curve == curve2 => "Unchanged"
            case _ => "Correction"
          }
        }

        groupedRates.get("New Curve").map{newCurves => MarketDataChange(day, key, newCurves.keySet.toList, isCorrection = false)}.toList :::
        groupedRates.get("Correction").map{newCurves => MarketDataChange(day, key, newCurves.keySet.toList, isCorrection = true)}.toList
      }}.toList
    }
    changes
  }
}

case class SpotFXDataEventSource(pricingGroup: PricingGroup, marketDataProvider: DataFlowDataProvider[Day, UOM, Quantity])
  extends PricingGroupMarketDataEventSource {

  type Key = Day
  type MarketType = UOM
  type CurveType = Quantity

  def marketDataEvent(change:MarketDataChange, key:Day, marketTypes: List[UOM], snapshot:SnapshotIDLabel): MarketDataEvent = {
    SpotFXDataEvent(change.observationDay, marketTypes, snapshot, change.isCorrection)
  }
}


trait DataFlowDataProvider[K, MarketType, CurveType]{
  def marketDataFor(pricingGroup : PricingGroup, version: Int, observationDay: Day): NestedMap[K, MarketType, CurveType]
}

case class SpotFXDataProvider (marketDataStore : MarketDataStore) extends DataFlowDataProvider[Day, UOM, Quantity] {
  private val titanCurrencies = EDMConversions.starlingUomSymbolToEdmUom.mapKeys(UOM.asUOM)
    .filterKeys(uom => uom.isCurrency && uom != UOM.USD)
  private val marketDataKeys: Some[Set[MarketDataKey]] = Some(titanCurrencies.keySet.map(SpotFXDataKey(_)))


  def marketDataFor(pricingGroup: PricingGroup, version:Int, observationDay: Day) = {
    val selection = MarketDataSelection(Some(pricingGroup))

    val data = marketDataStore.query(MarketDataIdentifier(selection, SpecificMarketDataVersion(version)), SpotFXDataType.name,
      observationDays = Some(Set(Some(observationDay))),
      marketDataKeys = marketDataKeys).map { case (timedKey, data) =>

      (timedKey.day, timedKey.key.asInstanceOf[SpotFXDataKey].ccy, data.asInstanceOf[SpotFXData].rate)
    }

    data.collect {
      case (Some(observationDay), currency, rate) => (observationDay, (currency, rate))
    }.groupInto(_._1, _._2).mapValues(_.toMap)
  }
}


case class ReferenceInterestDataProvider (marketDataStore : MarketDataStore) extends DataFlowDataProvider[(Day, String), UOM, PriceFixingsHistoryData]{
  def marketDataFor(pricingGroup : PricingGroup, version:Int, observationDay: Day) = {

    val identifier = MarketDataIdentifier(MarketDataSelection(Some(pricingGroup)), SpecificMarketDataVersion(version))

    val fixings: List[(Option[Day], Option[String], String, PriceFixingsHistoryData)] =
      marketDataStore.query(identifier, PriceFixingsHistoryDataType.name, observationDays = Some(Set(Some(observationDay))))
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

  def marketDataEvent(change:MarketDataChange, key:(Day,String), marketTypes: List[UOM], snapshot:SnapshotIDLabel): MarketDataEvent = {
    ReferenceInterestRateDataEvent(change.observationDay, key._2, marketTypes, snapshot, change.isCorrection)
  }
}