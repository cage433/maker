package starling.metals.datasources

import collection.immutable.List

import starling.daterange._
import starling.market._
import starling.marketdata._
import starling.utils.ImplicitConversions._
import starling.scheduler.ScheduledTime._
import starling.db.{MarketDataStore, MarketDataEntry}
import starling.gui.api._
import starling.quantity.{UOMSymbol, Quantity, UOM}
import starling.databases.{AbstractMarketDataProvider, MarketDataChange, PricingGroupMarketDataEventSource, MarketDataProvider}
import starling.lim.LIMService
import starling.utils.Pattern._
import Level._
import LIMService.TopRelation._
import ObservationTimeOfDay._
import starling.scheduler.{ScheduledTime, ScheduledTask, TaskDescription}
import starling.services.EmailService
import starling.market.FuturesExchangeFactory._
import starling.curves.readers.VerifyMarketDataAvailable
import scalaz.Scalaz._


object SpotFXLimMarketDataSource {
  val spotFXSources = SpotFXDataType.name → List(BloombergGenericFXRates, CFETSSpotFXFixings)

  val titanCurrencies = UOMSymbol.edmCurrencies.map(UOM.asUOM(_)).filter(_ != UOM.USD)
}

case class SpotFXLimMarketDataSource(service: LIMService, emailService: EmailService, template: Email)
  extends LimMarketDataSource(service) {

  import SpotFXLimMarketDataSource._

  override def description = List(spotFXSources).flatMap
    { case (marketDataType, sources) => marketDataType.name.pair(sources.flatMap(_.description)).map("%s → %s" % _) }

  def read(day: Day) = log.infoWithTime("Getting data from LIM") {
    Map(getValuesForType(SpotFXDataType.name, day.startOfFinancialYear, day, spotFXSources))
  }

  override def eventSources(marketDataStore: MarketDataStore) = List(
    SpotFXDataEventSource(PricingGroup.Metals, SpotFXMarketDataProvider(marketDataStore))
  )

  override def availabilityTasks(marketDataStore: MarketDataStore) = List(
    task("Bloomberg Generic Rate", limDaily(LME.calendar, 18 H 00), (titanCurrencies - UOM.CNY), marketDataStore),
    task("CFETS", limDaily(SHFE.calendar, 15 H 30), List(UOM.CNY), marketDataStore)
  )

  private def task(name: String, time: ScheduledTime, currencies: List[UOM], marketDataStore: MarketDataStore): TaskDescription =
    TaskDescription("Verify %s Spot FX Available" % name, time, task(name, marketDataStore, currencies))

  private def task(name: String, marketDataStore: MarketDataStore, currencies: List[UOM]): ScheduledTask =
    new VerifyMarketDataAvailable(marketDataStore, MarketDataSelection(Some(PricingGroup.Metals)), SpotFXDataType.name,
      currencies.map(SpotFXDataKey(_)).toSet, emailService, template) {

      protected def emailFor(observationDay: Day) = queryLatest(observationDay) match {
        case Nil => Some(template.copy(subject = "No Spot FX for: %s on %s" % (name, observationDay),
                                       body = currencies.map("MISSING: " + _).sorted.mkHtml()))

        case rates => {
          val rateAvailability = {
            val availableRates = rates.map(_._1.key.asInstanceOf[SpotFXDataKey].ccy).toSet

            currencies.groupBy(availableRates.contains(_) ? "Present" | "MISSING")
          }

          rateAvailability.contains("MISSING").option {
            template.copy(subject = "Missing Spot FX for: %s on %s" % (name, observationDay),
                          body = rateAvailability.flatMultiMap { _.format("%s: %s") }.toList.sorted.mkHtml())
          }
        }
      }
    }
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

case class SpotFXDataEventSource(pricingGroup: PricingGroup, provider: MarketDataProvider[Day, UOM, Quantity])
  extends PricingGroupMarketDataEventSource {

  type Key = Day
  type MarketType = UOM
  type CurveType = Quantity

  protected def marketDataEvent(change:MarketDataChange, key:Day, marketTypes: List[UOM], snapshot:SnapshotIDLabel) = {
    Some(SpotFXDataEvent(change.observationDay, marketTypes, snapshot, change.isCorrection))
  }

  protected def marketDataProvider = Some(provider)
}

case class SpotFXMarketDataProvider(marketDataStore : MarketDataStore) extends
  AbstractMarketDataProvider[Day, UOM, Quantity](marketDataStore) {


  val marketDataType = SpotFXDataType.name
  val marketDataKeys: Some[Set[MarketDataKey]] = Some(SpotFXLimMarketDataSource.titanCurrencies.map(SpotFXDataKey(_)).toSet)

  def marketDataFor(timedData: List[(TimedMarketDataKey, MarketData)]) = timedData.collect {
    case (TimedMarketDataKey(ObservationPoint(Some((observationDay, _))), SpotFXDataKey(currency)), SpotFXData(rate)) =>
      (observationDay, (currency, rate))
  }.toNestedMap
}