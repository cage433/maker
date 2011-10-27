package starling.curves.readers

import starling.market.Market
import starling.lim.LIMService
import starling.db.MarketDataStore
import starling.daterange.{Day, TemporalSpec}
import Market._
import Day._
import org.jmock.Expectations._
import starling.marketdata._

import starling.gui.api.{EmailEvent, MarketDataSelection}
import starling.utils._
import org.scalatest.matchers.ShouldMatchers

class PriceLimMarketDataSourceTests extends StarlingFixture with JMocker with TemporalSpec {
  private val observationDay = 27 Oct 2011
  private val observationDays = Option(Set(Option(observationDay)))
  override protected val frozenDay = Some(observationDay)

  observationDay.toString should "be a business day in all calendars" in { _ => import ShouldMatchers._
    List(cals.LME, cals.COMEX, cals.SFS).filterNot(_.isBusinessDay(observationDay)) should be === Nil
  }

  "DataSource" should "broadcast emails when data is missing" in { util => import util._
    val (dataStore, broadcaster, dataSource) = create(util)

    expecting { e => import e._
      allowing(dataStore).queryLatest(
        any[MarketDataSelection], any[MarketDataTypeName], withArg(equal(observationDays)), withArg(equal(None)), any[Option[Set[MarketDataKey]]]
      ); will(returnValue(noData))

      oneOf(broadcaster).broadcast(withArg(equal(email.copy(subject = "No Prices for: LME Metals on 27Oct2011"))))
      oneOf(broadcaster).broadcast(withArg(equal(email.copy(subject = "No Prices for: COMEX Metals on 27Oct2011"))))
      oneOf(broadcaster).broadcast(withArg(equal(email.copy(subject = "No Prices for: SHFE Metals on 27Oct2011"))))
    }

    whenExecuting {
      dataSource.availabilityTasks(dataStore).foreach(_.run)
    }
  }

  private def create(util: JMockUtil) = { import util._
    val dataStore = mock[MarketDataStore]
    val broadcaster = mock[Broadcaster]

    (dataStore, broadcaster, new PriceLimMarketDataSource(LIMService.Null, cals, broadcaster, email.from, email.to))
  }

  private def noData = List.empty[(TimedMarketDataKey, MarketData)]
  private def email = EmailEvent(from = "<sender>", to = "<recipient")
}