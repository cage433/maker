package starling.services.rpc.marketdata

import starling.utils.{JMocker, StarlingSpec}
import starling.services.rpc.valuation.EnvironmentProvider
import com.trafigura.services.valuation.TitanMarketDataIdentifier
import starling.utils.ImplicitConversions._
import org.scalatest.matchers.ShouldMatchers
import starling.db.{SnapshotID, MarketDataStore}
import org.jmock.Expectations._
import starling.marketdata._
import starling.quantity.{Quantity, UOMSymbol, UOM}
import starling.titan.EDMConversions._
import com.trafigura.services.TitanSerializableCurrency
import com.trafigura.services.marketdata.{MarketDataServiceApi, SpotFXRate}
import starling.gui.api.{PricingGroup, MarketDataIdentifier}
import starling.market.Market
import starling.daterange.{Location, ObservationTimeOfDay, Day, TemporalSpec}
import starling.calendar.{WeekdayBusinessCalendar, Clock}
import Day._

class MarketDataServiceTests extends StarlingSpec with JMocker with TemporalSpec {
  import context._; import expectations._

  implicit def enrichSpotFXRate(rate: SpotFXRate) = new {
    import scalaz.Scalaz._

    def between = try {
      rate.rate.uom.reverse |> (uom => (UOM.fromIdentifier(uom(1)), UOM.fromIdentifier(uom(-1))))
    } catch { case _ =>
      throw new Exception("Rate without numerator or denominator: " + rate)
    }
  }

  "getSpotFXRates" should {
    "return all crosses of titan currencies (including reciprocals but not trivial crosses)" in {
      val (marketDataStore, marketDataService) = create

      expecting {
        oneOf(marketDataStore).query(any[MarketDataIdentifier], withArg(equal(SpotFXDataType.name)),
          withArg(equal(observationDays)), withArg(equal(noObservationTimes)), withArg(equal(noMarketDataKeys)))
        will(returnValue(dataForAllCurrenciesFor(observationDay)))
      }

      val crosses = whenExecuting {
        marketDataService.getSpotFXRates(marketDataId).map(_.between).toSet
      }

      import ShouldMatchers._

      (nonTrivialCrosses -- crosses) should be === Set()
    }
  }

  "getSpotFXRate" should {
    "be able to return a rate for any cross of titan currencies (including trivial crosses & inverses)" in {
      val (marketDataStore, marketDataService) = create

      expecting {
        allowing(marketDataStore).query(any[MarketDataIdentifier], withArg(equal(SpotFXDataType.name)),
          withArg(equal(observationDays)), withArg(equal(noObservationTimes)), any[Option[Set[MarketDataKey]]])
        will(returnValue(dataForAllCurrenciesFor(observationDay)))
      }

      whenExecuting {
        allCrosses.foreach { case (from, to) =>
          marketDataService.getSpotFXRate(marketDataId, from.serializableCurrency.get, to.serializableCurrency.get)
        }
      }
    }
  }

  "getLatestSpotFXRate" should {
    "return rates for the most recent snapshot & observation day that includes all Titan currencies" in {
      val (marketDataStore, marketDataService) = create

      expecting {
        oneOf(marketDataStore).latestSnapshot(any[PricingGroup]); will(returnValue(Some(snapshot)))

        oneOf(marketDataStore).query(any[MarketDataIdentifier], withArg(equal(SpotFXDataType.name)),
          withArg(equal(Some(lastBusinessWeek.map(Option(_)).toSet))), withArg(equal(noObservationTimes)), any[Option[Set[MarketDataKey]]])
        will(returnValue(dataForSomeCurrenciesFor(observationDay.addBusinessDays(weekDay, -4)) :::
                         dataForAllCurrenciesFor(observationDay.addBusinessDays(weekDay, -5))))
      }

      val rates = whenExecuting {
        marketDataService.latestSpotFXRates
      }

      import ShouldMatchers._
      rates.map(_.marketDataID.observationDay).distinct should be === List(observationDay.addBusinessDays(weekDay, -5))
    }

    "fail if no complete set of rates can be found in the last business week" in {
      val (marketDataStore, marketDataService) = create

      expecting {
        oneOf(marketDataStore).latestSnapshot(any[PricingGroup]); will(returnValue(Some(snapshot)))

        oneOf(marketDataStore).query(any[MarketDataIdentifier], withArg(equal(SpotFXDataType.name)),
          withArg(equal(Some(lastBusinessWeek.map(Option(_)).toSet))), withArg(equal(noObservationTimes)), any[Option[Set[MarketDataKey]]])
        will(returnValue(dataForAllCurrenciesFor(observationDay.addBusinessDays(weekDay, -6))))
      }

      val exception = whenExecuting {
        intercept[IllegalArgumentException] {
          marketDataService.latestSpotFXRates
        }
      }

      import ShouldMatchers._

      exception.getMessage should be === "No complete set of Spot FX rates within the last business week"
    }

    "fail if no snapshots have been made in the last week" in {
      val (marketDataStore, marketDataService) = create

      expecting {
        oneOf(marketDataStore).latestSnapshot(any[PricingGroup]); will(returnValue(None))
      }

      val exception = whenExecuting {
        intercept[IllegalArgumentException] {
          marketDataService.latestSpotFXRates
        }
      }

      import ShouldMatchers._

      exception.getMessage should be === "No Market Data Snapshots have been made within the last business week"
    }
  }

  private def create = {
    val marketDataStore = mock[MarketDataStore]

    expecting {
      allowing(marketDataStore).snapshotFromID(withArg(equal(snapshotId))); will(returnValue(Some(snapshot)))
    }

    (marketDataStore, new MarketDataService(marketDataStore).asInstanceOf[MarketDataServiceApi])
  }

  private val weekDay = new WeekdayBusinessCalendar(Location.Unknown)
  private val observationDay = 2 Nov 2011
  private val lastBusinessWeek = observationDay.businessDays(weekDay, (-5).to(0)).toList
  private val observationDays = Option(Set(Option(observationDay)))
  private val snapshotId = 1
  private val snapshot = SnapshotID(snapshotId, Clock.timestamp, null, null, None, 1)
  private val marketDataId = TitanMarketDataIdentifier.valueOf(snapshotId + "-2011-11-02"+ "-2011-11-02")
  private val expectedCurrencies = UOMSymbol.edmCurrencies.map(UOM.asUOM(_)).toSet

  private val (nonTrivialCrosses, noObservationTimes, noMarketDataKeys) = {
    import scalaz.Scalaz._

    val expectedCrosses = (expectedCurrencies <|*|> expectedCurrencies).filterNot(p => p._1 == p._2)

    (expectedCrosses, none[Set[ObservationTimeOfDay]], none[Set[MarketDataKey]])
  }

  private val allCrosses = nonTrivialCrosses ++ expectedCurrencies.map(c => (c,c))
  private val noData = List.empty[(TimedMarketDataKey, MarketData)]

  private def dataForSomeCurrenciesFor(day: Day) = dataFor(day, expectedCurrencies -- List(UOM.USD, UOM.GBP))
  private def dataForAllCurrenciesFor(day: Day) = dataFor(day, expectedCurrencies - UOM.USD)

  private def dataFor(day: Day, currencies: Set[UOM]) = currencies.map { ccy =>
    TimedMarketDataKey(day.atTimeOfDay(ObservationTimeOfDay.LondonClose), SpotFXDataKey(ccy)) →
      SpotFXData(Quantity(1.0, ccy / UOM.USD))
  }.toList

  override protected val frozenDay = Some(observationDay)
}
