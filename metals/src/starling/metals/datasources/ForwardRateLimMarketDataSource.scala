package starling.metals.datasources

import starling.daterange._
import starling.market._
import starling.marketdata._
import FuturesExchangeFactory._
import starling.utils.ImplicitConversions._
import starling.gui.api._
import starling.lim.LIMService
import scalaz.Scalaz._
import starling.services.EmailService
import starling.scheduler.{EmailingScheduledTask, TaskDescription}
import starling.utils.ClosureUtil._
import starling.db.{NormalMarketDataReader, MarketDataStore}
import starling.quantity.{Quantity, UOM}
import collection.immutable.{Map, List}
import org.joda.time.Period
import starling.utils.ImplicitConversions
import starling.databases.{MarketDataChange, PricingGroupMarketDataEventSource, MarketDataProvider, AbstractMarketDataProvider}


case class ForwardRateLimMarketDataSource(service: LIMService, emailService: EmailService, template: Email)
  extends LimMarketDataSource(service, ForwardRateDataType.name) {
  private val sources = List(LIBORFixingsSource)

  override def description = descriptionFor(sources)
  def read(day: Day) = Map(getValuesForType(earliestDayToImport(day), day, sources))

  override def availabilityTasks(marketDataStore: MarketDataStore) = List(
    TaskDescription("Verify SIBOR Available", limDaily(LME.calendar, 4 H 30), notImplemented),
    TaskDescription("Verify ROBOR Available", limDaily(LME.calendar, 9 H 00), notImplemented),
    TaskDescription("Verify JIBAR Available", limDaily(LME.calendar, 10 H 30), notImplemented),
    TaskDescription("Verify Libor maturities available", limDaily(LME.calendar, 12 H 00),
      new VerifyLiborMaturitiesAvailable(marketDataStore, emailService, template).withSource("LIM")),
    TaskDescription("Verify IRS Available", limDaily(LME.calendar, 18 H 00), notImplemented),

    TaskDescription("Verify SHIBOR Available", limDaily(NYMEX.calendar, 0 H 00), notImplemented),
    TaskDescription("Verify MIBOR-1 Available", limDaily(NYMEX.calendar, 1 H 00), notImplemented),
    TaskDescription("Verify AIDIBOR Available", limDaily(NYMEX.calendar, 3 H 00), notImplemented),
    TaskDescription("Verify MIBOR-2 Available", limDaily(NYMEX.calendar, 5 H 00), notImplemented),
    TaskDescription("Verify BUBOR Available", limDaily(NYMEX.calendar, 05 H 00), notImplemented),
    TaskDescription("Verify SOFIBOR Available", limDaily(NYMEX.calendar, 05 H 00), notImplemented),
    TaskDescription("Verify TRLIBOR Available", limDaily(NYMEX.calendar, 6 H 00), notImplemented),
    TaskDescription("Verify NIBOR Available", limDaily(NYMEX.calendar, 7 H 00), notImplemented),
    TaskDescription("Verify KLIBOR Available", limDaily(NYMEX.calendar, 23 H 00), notImplemented),

    TaskDescription("Verify HIBOR Available", limDaily(SHFE.calendar, 11 H 00), notImplemented)
  ).filterNot(_.task == notImplemented)

  override def eventSources(marketDataStore: MarketDataStore) = {
    List(new ForwardRateDataEventSource(PricingGroup.Metals, ReferenceInterestMarketDataProvider(marketDataStore)))
  }
}

class VerifyLiborMaturitiesAvailable(marketDataStore: MarketDataStore, emailService: EmailService, template: Email)
  extends EmailingScheduledTask(emailService, template) {

  import LIBORCalculator._
  private val currencies = LIBORCalculator.currencies(ForwardRateSource.LIBOR)

  protected def emailFor(observationDay: Day) = {
    val liborFixings: NestedMap[UOM, Tenor, (Quantity, Day)] = latestLiborFixings(marketDataStore, observationDay)
    val tenorsByCurrency = liborFixings.mapValues(_.keys.toList).withDefaultValue(Nil)
    val missingTenorsByCurrency = currencies.toMapWithValues(currency =>
      tenorsFor(currency) \\ tenorsByCurrency(currency)).filterValuesNot(_.isEmpty).sortKeysBy(_.toString)

    (missingTenorsByCurrency.size > 0).option {
      template.copy(subject = "Missing Libor Maturities in LIM, observation day: " + observationDay,
        body = <span>
                 <p>The following LIBOR tenors are required by Trinity but are missing in LIM</p>
                 <table>
                   { for ((currency, missingTenors) <- missingTenorsByCurrency) yield
                     <tr><td><b>{currency}: </b></td><td>{missingTenors.mkString(", ")}</td></tr>
                   }
                 </table>
               </span>.toString)
    }
  }

  def latestLiborFixings(marketDataStore: MarketDataStore, observationDay: Day): NestedMap[UOM, Tenor, (Quantity, Day)] =
    liborFixingsHistoryData(marketDataStore, observationDay).mapNestedValues((_, observationDay))

  private def liborFixingsHistoryData(marketDataStore: MarketDataStore, observationDay: Day): NestedMap[UOM, Tenor, Quantity] =
    currencies.toMapWithSomeValues(currency => safely(read(marketDataStore, observationDay, ForwardRateDataKey(currency))).toOption.flatOpt)

  private def read(marketDataStore: MarketDataStore, observationDay: Day, key: ForwardRateDataKey): Option[Map[Tenor, Quantity]] =
    latestLimOnlyMarketDataReader(marketDataStore).readAs[ForwardRateData](TimedMarketDataKey(
      observationDay.atTimeOfDay(key.observationTime), key)).rates.get(ForwardRateSource.LIBOR)

  private def latestLimOnlyMarketDataReader(marketDataStore: MarketDataStore) = new NormalMarketDataReader(
    marketDataStore, marketDataStore.latestMarketDataIdentifier(MarketDataSelection(Some(PricingGroup.Metals))))
}

case class ReferenceInterestMarketDataProvider(marketDataStore : MarketDataStore)
  extends AbstractMarketDataProvider[ForwardRateSource, UOM, Map[Tenor, Quantity]](marketDataStore) {

  val marketDataType = ForwardRateDataType.name
  val marketDataKeys = None

  def marketDataFor(timedData: List[(TimedMarketDataKey, MarketData)]) = {
    val x: List[(UOM, ImplicitConversions.NestedMap[ForwardRateSource, Tenor, Quantity])] = timedData.collect {
      case (TimedMarketDataKey(_, ForwardRateDataKey(currency)), ForwardRateData(data)) => (currency, data)
    }

    val y: Map[UOM, Map[ForwardRateSource, Map[Tenor, Quantity]]] = x.toMap

    y.flipNesting
  }
}

case class ForwardRateDataEventSource(pricingGroup: PricingGroup,
  provider: MarketDataProvider[ForwardRateSource, UOM, Map[Tenor, Quantity]]) extends PricingGroupMarketDataEventSource {

  type Key = ForwardRateSource
  type MarketType = UOM
  type CurveType = Map[Tenor, Quantity]

  protected def marketDataEvent(change: MarketDataChange, source: ForwardRateSource, currencies: List[UOM], snapshot: SnapshotIDLabel) = {
    Some(ReferenceInterestRateDataEvent(change.observationDay, source.value, currencies, snapshot, change.isCorrection))
  }

  protected def marketDataProvider = Some(provider)
}

