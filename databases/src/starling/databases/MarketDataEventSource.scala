package starling.databases

import starling.gui.api._
import starling.daterange.Day
import starling.utils.ImplicitConversions._
import scalaz.Scalaz._
import starling.db.MarketDataStore
import starling.marketdata.{MarketData, TimedMarketDataKey, MarketDataKey, MarketDataTypeName}

trait MarketDataEventSource {
  type Key
  type MarketType
  type CurveType
  def matches(pricingGroup: PricingGroup): Boolean
  def changesFor(previousVersion: Int, newVersion: Int, observationDays: List[Day]): List[MarketDataChange]
  protected def marketDataProvider: Option[MarketDataProvider[Key, MarketType, CurveType]]
  protected def marketDataEvent(change:MarketDataChange, key:Key, marketTypes: List[MarketType], snapshot:SnapshotIDLabel): Option[MarketDataEvent]

  protected final def change(day: Day, key: Key, marketTypes: Traversable[MarketType], isCorrection: Boolean = false) =
    new MarketDataChange(day, isCorrection) {
      def eventFor(snapshot:SnapshotIDLabel): Option[MarketDataEvent] = marketDataEvent(this, key, marketTypes.toList, snapshot)
    }
}

trait NullMarketDataEventSource extends MarketDataEventSource {
  type Key = Nothing
  type MarketType = Nothing
  type CurveType = Nothing

  def matches(pricingGroup:PricingGroup) = false
  def changesFor(previousVersion: Int, newVersion: Int, observationDays: List[Day]): List[MarketDataChange] = Nil
  protected def marketDataProvider: Option[MarketDataProvider[Key, MarketType, CurveType]] = None
  protected def marketDataEvent(change:MarketDataChange, key:Nothing, marketTypes: List[Nothing], snapshot:SnapshotIDLabel) = None
}

abstract class MarketDataChange(val observationDay: Day, val isCorrection: Boolean) {
  def eventFor(snapshot: SnapshotIDLabel): Option[MarketDataEvent]
}

trait MarketDataProvider[Key, MarketType, CurveType] {
  def marketDataFor(pricingGroup : PricingGroup, version: Int, observationDay: Day): NestedMap[Key, MarketType, CurveType]
}

abstract class AbstractMarketDataProvider[Key, MarketType, CurveType](marketDataStore: MarketDataStore)
  extends MarketDataProvider[Key, MarketType, CurveType] {

  val marketDataType: MarketDataTypeName
  val marketDataKeys: Option[Set[MarketDataKey]]

  def marketDataFor(pricingGroup: PricingGroup, version: Int, observationDay: Day) = marketDataFor(
    marketDataStore.query(MarketDataIdentifier(MarketDataSelection(Some(pricingGroup)), SpecificMarketDataVersion(version)),
      marketDataType, Some(Set(Some(observationDay))), None, marketDataKeys))

  def marketDataFor(timedData: List[(TimedMarketDataKey, MarketData)]): NestedMap[Key, MarketType, CurveType]
}

abstract class PricingGroupMarketDataEventSource extends MarketDataEventSource {
  val pricingGroup: PricingGroup
  def matches(pg: PricingGroup) = (pg == pricingGroup)

  def changesFor(previousVersion: Int, newVersion: Int, observationDays: List[Day]) = marketDataProvider.map { provider => {
    val changes = observationDays.flatMap { day =>
      val previousCurves: NestedMap[Key, MarketType, CurveType] = provider.marketDataFor(pricingGroup, previousVersion, day)
      val curves: NestedMap[Key, MarketType, CurveType] = provider.marketDataFor(pricingGroup, newVersion, day)

      curves.flatMap { case (key, curveByMarketType) => {
        val groupedRates = curveByMarketType.groupBy{
          case (marketType, curve) => previousCurves.getOrElse(key, Map[MarketType, CurveType]()).get(marketType) match {
            case None => "New Curve"
            case Some(curve2) if curve == curve2 => "Unchanged"
            case _ => "Correction"
          }
        }

        groupedRates.get("New Curve").map{newCurves => change(day, key, newCurves.keySet, isCorrection = false)}.toList :::
        groupedRates.get("Correction").map{newCurves => change(day, key, newCurves.keySet, isCorrection = true)}.toList
      }}.toList
    }

    changes
  } } | Nil
}