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
import starling.lim.{LIMConnection, LIMService}
import starling.utils.ImplicitConversions


object SpotFXLimMarketDataSource {
  val sources = List(BloombergGenericFXRates)

  val titanCurrencies = UOMSymbol.currenciesToImportFromLIM.map(UOM.asUOM(_)).filter(_ != UOM.USD)
}

case class SpotFXLimMarketDataSource(service: LIMService, emailService: EmailService, template: Email)
  extends LimMarketDataSource(service, SpotFXDataType.name) {

  import SpotFXLimMarketDataSource._

  override def description = descriptionFor(sources)
  def read(day: Day) = Map(getValuesForType(earliestDayToImport(day), day, sources))

  override def eventSources(marketDataStore: MarketDataStore) = List(
    SpotFXDataEventSource(PricingGroup.Metals, SpotFXMarketDataProvider(marketDataStore, titanCurrencies))
  )

  override def availabilityTasks(marketDataStore: MarketDataStore) = List(
    task("Bloomberg Generic Rate", limDaily(LME.calendar, 21 H 00), (titanCurrencies filterNot(_ == UOM.CNY)), marketDataStore),
    task("CFETS", limDaily(SHFE.calendar, 22 H 00), List(UOM.CNY), marketDataStore)
  //      registerTasks(tasks(limDaily(LME, 13 H 15), TRAF.LME.{EUR, GBP, JPY} SpotFX
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

object BloombergGenericFXRates extends LimSource {
  def description = List(Trafigura.Bloomberg.Currencies.Composite.name + " TRAF.BGNL.* (Close)")

  def marketDataEntriesFrom(connection: LIMConnection, start: Day, end: Day) = {
    val prices: ImplicitConversions.NestedMap[Day, Relation, Double] =
      connection.getPrices(fxRelations, Level.Close, start, end)

    prices.mapNested {
      case (observationDay, relation, rate) => relation.entryFor(observationDay.atTimeOfDay(LondonClose), rate)
    }
  }

  private def fxRelations = currencies.map(Relation(UOM.USD, _)) ++ currencies.map(Relation(_, UOM.USD))
  private def currencies = UOM.currencies.filterNot(_.isOneOf(UOM.CNY, UOM.USD))

  private case class Relation(from: UOM, to: UOM) {
    require(from == UOM.USD || to == UOM.USD)

    def limRelation = "TRAF.BGNL.%s%s" % (from, to)

    def entryFor(observationPoint: ObservationPoint, rate: Double) = MarketDataEntry(observationPoint,
      SpotFXDataKey((from == UOM.USD) ? to | from), SpotFXData(Quantity(rate, to / from)))
  }
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

case class SpotFXMarketDataProvider(marketDataStore: MarketDataStore, currencies: List[UOM]) extends
  AbstractMarketDataProvider[Day, UOM, Quantity](marketDataStore) {


  val marketDataType = SpotFXDataType.name
  val marketDataKeys: Some[Set[MarketDataKey]] = Some(currencies.map(SpotFXDataKey(_)).toSet)

  def marketDataFor(timedData: List[(TimedMarketDataKey, MarketData)]) = timedData.collect {
    case (TimedMarketDataKey(ObservationPoint(Some((observationDay, _))), SpotFXDataKey(currency)), SpotFXData(rate)) =>
      (observationDay, (currency, rate))
  }.toNestedMap
}