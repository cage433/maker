package starling.metals.datasources

import starling.marketdata._
import starling.utils._
import org.scalatest.matchers.ShouldMatchers
import starling.quantity.Quantity
import org.jmock.Expectations._
import starling.services.EmailService
import starling.gui.api.{Email, MarketDataSelection}
import starling.utils.ImplicitConversions._
import starling.market.FuturesExchangeFactory._
import starling.market.{Commodity, FuturesExchange, FuturesMarket, Market}
import starling.daterange.{TimeZone, Day, TemporalSpec}
import Day._
import Market._
import collection.immutable.Map
import starling.lim.{LimNode, LIMService}
import starling.db.{MarketDataSource, MarketDataStore}
import starling.scheduler.TaskDescription


abstract class LimMarketDataSourceTests[DS <: MarketDataSource] extends StarlingSpec with JMocker with TemporalSpec {
  import context._; import expectations._

  protected val observationDay = 27 Oct 2011
  protected val observationDays = Option(Set(Option(observationDay)))
  override protected val frozenDay = Some(observationDay)

  protected def createDataSource(emailService: EmailService): DS
  protected def expectEmailsForNoData(emailService: EmailService)
  protected def completeSetOfData: List[(TimedMarketDataKey, MarketData)]

  observationDay.toString should {
    "be a business day in all calendars" in {
      import ShouldMatchers._
      List(LME.calendar, COMEX.calendar, SHFE.calendar).filterNot(_.isBusinessDay(observationDay)) should be === Nil
    }
  }

  "DataSource" should {
    "send emails" when {
      "there's no data at all" in {
        val (dataStore, emailService, dataSource) = create

        expecting {
          dataStore.queryLatestReturns(noData)

          expectEmailsForNoData(emailService)
        }

        whenExecuting {
          dataSource.runAvailabiltyTasks
        }
      }
    }

    "send no emails when data present" in {
      val (dataStore, emailService, dataSource) = create

      expecting {
        dataStore.queryLatestReturns(completeSetOfData)
        never(emailService).send(any[Email])
      }

      whenExecuting {
        dataSource.runAvailabiltyTasks
      }
    }
  }

  class ProxyDataStore(dataStore: MarketDataStore) {
    def queryLatestReturns(data: List[(TimedMarketDataKey, MarketData)]) {
      allowing(dataStore).queryLatest(
        any[MarketDataSelection], any[MarketDataTypeName], withArg(equal(observationDays)), withArg(equal(None)), any[Option[Set[MarketDataKey]]]
      ); will(returnValue(data))
    }
  }

  class ProxyDataSource(dataSource: MarketDataSource, dataStore: MarketDataStore) {
    def runAvailabiltyTasks = dataSource.availabilityTasks(dataStore).foreach(_.run)
  }

  protected final def create = {
    val dataStore = mock[MarketDataStore]
    val emailService = mock[EmailService]

    (new ProxyDataStore(dataStore), emailService,
      new ProxyDataSource(createDataSource(emailService), dataStore))
  }


  protected final def noData = List.empty[(TimedMarketDataKey, MarketData)]
  protected final def template = Email(from = "<sender>", to = "<recipient")
  protected final def expectBodyWith(tagged: List[(String, AnyRef)]*) = tagged.toList.flatten.map { _.format("%s: %s") }.sorted.mkHtml()
  protected final def missing(markets: AnyRef*) = "MISSING".pairWith(markets).toList
  protected final def present(markets: AnyRef*) = "Present".pairWith(markets).toList
}