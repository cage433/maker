package starling.curves.readers

import collection.immutable.List

import starling.LIMServer
import starling.daterange._
import starling.market._
import starling.marketdata._
import starling.utils.Pattern

import Level._
import LIMServer.TopRelation._
import ObservationTimeOfDay._
import Pattern._
import starling.utils.ImplicitConversions._
import starling.scheduler.ScheduledTime._
import starling.scheduler.TaskDescription
import starling.calendar.BusinessCalendars
import starling.db.{MarketDataStore, MarketDataEntry}
import starling.gui.api._
import starling.quantity.{UOMSymbol, Quantity, UOM}
import starling.databases.{AbstractDataFlowDataProvider, MarketDataChange, PricingGroupMarketDataEventSource, DataFlowDataProvider}


object SpotFXLimMarketDataSource {
  val spotFXSources = SpotFXDataType.name → List(BloombergGenericFXRates, CFETSSpotFXFixings)
}

case class SpotFXLimMarketDataSource(limServer: LIMServer, calendars: BusinessCalendars) extends LimMarketDataSource(limServer) {
  import SpotFXLimMarketDataSource._

  override def description = List(spotFXSources).flatMap
    { case (marketDataType, sources) => marketDataType.name.pair(sources.flatMap(_.description)).map("%s → %s" % _) }

  def read(day: Day) = log.infoWithTime("Getting data from LIM") {
    Map(getValuesForType(SpotFXDataType.name, day.startOfFinancialYear, day, spotFXSources))
  }

  override def eventSources(marketDataStore: MarketDataStore) = {
    val spotfx = SpotFXDataEventSource(PricingGroup.Metals, SpotFXDataProvider(marketDataStore))

    List(spotfx)
  }

  override def availabilityTasks(marketDataStore: MarketDataStore) = List(
  //      registerTasks(tasks(daily(LME, 13 H 15), TRAF.LME.{EUR, GBP, JPY} SpotFX
    TaskDescription("Verify Bloomberg Generic FX Rates Available", daily(calendars.LME, 18 H 00), notImplemented),
    TaskDescription("Verify CFETSSpotFXFixings Available", daily(calendars.SFS, 15 H 30), notImplemented)
  )
}

object BloombergGenericFXRates extends HierarchicalLimSource(List(Trafigura.Bloomberg.Currencies.Composite), List(Close)) {
  type Relation = FXRelation

  case class FXRelation(from: UOM, to: UOM) {
    def againstUSD(rate: Double): Option[(UOM,Quantity)] = (from, to) partialMatch {
      case (UOM.USD, ccy) => (ccy, Quantity(rate, to / from))
      case (ccy, UOM.USD) => (ccy, Quantity(rate, to / from))
    }
  }

  def relationExtractor = Extractor.regex("""TRAF\.BGNL\.(...)(...)""") {
    case List(UOM.Parse(from), UOM.Parse(to)) => Some(FXRelation(from, to))
  }

  def marketDataEntriesFrom(allRates: List[Prices[FXRelation]]) = allRates.flatMap { rates =>
    rates.relation.againstUSD(rates.priceByLevel(Close)).filterNot(_._1 == UOM.CNY).toList.map{ case (ccy,fx) =>
      MarketDataEntry(rates.observationDay.atTimeOfDay(LondonClose), SpotFXDataKey(ccy), SpotFXData(fx))}
  }
}

object CFETSSpotFXFixings extends SpotFXFixings("SFS", SHFEClose, Close, UOM.USD, """TRAF\.CFETS\.(CNY)""",
  Trafigura.Bloomberg.Currencies.Composite) {

  override protected def key(currency: UOM) = SpotFXDataKey(currency)
  override protected def value(price: Double, currency: UOM) = SpotFXData(Quantity(price, currency / UOM.USD))
}

case class SpotFXDataEventSource(pricingGroup: PricingGroup, provider: DataFlowDataProvider[Day, UOM, Quantity])
  extends PricingGroupMarketDataEventSource {

  type Key = Day
  type MarketType = UOM
  type CurveType = Quantity

  protected def marketDataEvent(change:MarketDataChange, key:Day, marketTypes: List[UOM], snapshot:SnapshotIDLabel) = {
    Some(SpotFXDataEvent(change.observationDay, marketTypes, snapshot, change.isCorrection))
  }

  protected def marketDataProvider = Some(provider)
}

case class SpotFXDataProvider (marketDataStore : MarketDataStore) extends
  AbstractDataFlowDataProvider[Day, UOM, Quantity](marketDataStore) {

  private val titanCurrencies = UOMSymbol.edmCurrencies.map(UOM.asUOM(_)).filter(_ != UOM.USD)

  val marketDataType = SpotFXDataType.name
  val marketDataKeys: Some[Set[MarketDataKey]] = Some(titanCurrencies.map(SpotFXDataKey(_)).toSet)

  def marketDataFor(timedData: List[(TimedMarketDataKey, MarketData)]) = timedData.collect {
    case (TimedMarketDataKey(ObservationPoint(Some((observationDay, _))), SpotFXDataKey(currency)), SpotFXData(rate)) =>
      (observationDay, (currency, rate))
  }.toNestedMap
}