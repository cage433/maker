package starling.metals.datasources

import collection.immutable.List

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
import starling.quantity.{Percentage, UOM}
import starling.pivot.MarketValue
import starling.utils.ClosureUtil._
import starling.db.{NormalMarketDataReader, MarketDataStore}


case class ForwardRateLimMarketDataSource(service: LIMService, emailService: EmailService, template: Email)
  extends LimMarketDataSource(service, ForwardRateDataType.name) {

  def read(day: Day) = log.infoWithTime("Getting data from LIM") {
    Map(getValuesForType(day.startOfFinancialYear, day, List(LIBORFixings)))
  }

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
  )
}

class VerifyLiborMaturitiesAvailable(marketDataStore: MarketDataStore, emailService: EmailService, template: Email)
  extends EmailingScheduledTask(emailService, template) {

  import LIBORCalculator._

  protected def emailFor(observationDay: Day) = {
    val liborFixings: NestedMap[UOM, Tenor, (Percentage, Day)] = latestLiborFixings(marketDataStore, observationDay)
    val tenorsByCurrency = liborFixings.mapValues(_.keys.toList).withDefaultValue(Nil)
    val missingTenorsByCurrency = currencies.toMapWithValues(currency => tenorsFor(currency) \\ tenorsByCurrency(currency))
      .filterValuesNot(_.isEmpty).sortKeysBy(_.toString)

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

  def latestLiborFixings(marketDataStore: MarketDataStore, observationDay: Day): NestedMap[UOM, Tenor, (Percentage, Day)] = {
    liborFixingsHistoryData(marketDataStore, observationDay).mapValues(_.fixingsFor(periods).collect {
      case ((Level.Close, StoredFixingPeriod.Tenor(tenor)), MarketValue.Percentage(percentage)) => {
        tenor → (percentage, observationDay)
      }
    })
  }

  private def liborFixingsHistoryData(marketDataStore: MarketDataStore, observationDay: Day): Map[UOM, PriceFixingsHistoryData] =
    currencies.toMapWithSomeValues(currency => safely(read(marketDataStore, observationDay, currency)).toOption)

  private def read(marketDataStore: MarketDataStore, observationDay: Day, currency: UOM) =
    latestLimOnlyMarketDataReader(marketDataStore).readAs[PriceFixingsHistoryData](TimedMarketDataKey(
      observationDay.atTimeOfDay(ObservationTimeOfDay.LiborClose), PriceFixingsHistoryDataKey(currency.toString, Some("LIBOR"))))

  private def latestLimOnlyMarketDataReader(marketDataStore: MarketDataStore) = new NormalMarketDataReader(
    marketDataStore, marketDataStore.latestMarketDataIdentifier(MarketDataSelection(Some(PricingGroup.Metals))))
}