package starling.services.rpc.marketdata

import starling.utils.{JMocker, StarlingSpec}
import starling.services.rpc.valuation.EnvironmentProvider
import com.trafigura.services.valuation.TitanMarketDataIdentifier
import com.trafigura.services.marketdata.SpotFXRate
import starling.utils.ImplicitConversions._
import org.scalatest.matchers.ShouldMatchers
import starling.db.{SnapshotID, MarketDataStore}
import starling.calendar.Clock
import org.jmock.Expectations._
import starling.gui.api.MarketDataIdentifier
import starling.daterange.{ObservationTimeOfDay, Day, TemporalSpec}
import Day._
import starling.marketdata._
import starling.quantity.{Quantity, UOMSymbol, UOM}
import starling.titan.EDMConversions._
import com.trafigura.services.TitanSerializableCurrency

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
        will(returnValue(dataForAllCurrencies))
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
        will(returnValue(dataForAllCurrencies))
      }

      whenExecuting {
        allCrosses.foreach { case (from, to) =>
          marketDataService.getSpotFXRate(marketDataId, from.serializableCurrency.get, to.serializableCurrency.get)
        }
      }
    }
  }

  private def create = {
    val marketDataStore = mock[MarketDataStore]

    expecting {
      allowing(marketDataStore).snapshotFromID(withArg(equal(snapshotId)))
        will(returnValue(Some(SnapshotID(1, Clock.timestamp, null, null, 1))))
    }

    (marketDataStore, new MarketDataService(marketDataStore))
  }

  private val observationDay = 2 Nov 2011
  private val observationDays = Option(Set(Option(observationDay)))

  private val snapshotId = 1
  private val marketDataId = TitanMarketDataIdentifier.valueOf(snapshotId + "-2011-11-02")
  private val expectedCurrencies = UOMSymbol.edmCurrencies.map(UOM.asUOM(_)).toSet
  private val (nonTrivialCrosses, noObservationTimes, noMarketDataKeys) = {
    import scalaz.Scalaz._

    val expectedCrosses = (expectedCurrencies <|*|> expectedCurrencies).filterNot(p => p._1 == p._2)

    (expectedCrosses, none[Set[ObservationTimeOfDay]], none[Set[MarketDataKey]])
  }

  private val allCrosses = nonTrivialCrosses ++ expectedCurrencies.map(c => (c,c))

  private val dataForAllCurrencies = (expectedCurrencies - UOM.USD).map { ccy =>
    TimedMarketDataKey(observationDay.atTimeOfDay(ObservationTimeOfDay.LondonClose), SpotFXDataKey(ccy)) →
      SpotFXData(Quantity(1.0, ccy / UOM.USD))
  }.toList

  override protected val frozenDay = Some(observationDay)
}