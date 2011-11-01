package starling.metals.datasources

import org.jmock.Expectations._
import starling.services.EmailService
import starling.lim.LIMService
import starling.daterange.ObservationTimeOfDay
import starling.utils.ImplicitConversions._
import starling.quantity.{UOM, Quantity}
import starling.marketdata.{MarketData, SpotFXData, SpotFXDataKey, TimedMarketDataKey}


class SpotFXLimMarketDataSourceTests extends LimMarketDataSourceTests[SpotFXLimMarketDataSource] {
  import SpotFXLimMarketDataSource._; import UOM._
  import context._; import expectations._

  "DataSource should send emails when there's missing data for a currency" in {
    val (dataStore, emailService, dataSource) = create

    expecting {
      dataStore.queryLatestReturns(ratesForEveryCurrencyExcept(GBP))

      oneOf(emailService).send(withArg(equal(template.copy(subject = "Missing Spot FX for: Bloomberg Generic Rate on 27Oct2011",
        body = expectBodyWith(missing(GBP), present(titanCurrencies -- List(CNY, GBP) : _*))))))
    }

    whenExecuting {
      dataSource.runAvailabiltyTasks
    }
  }

  protected def createDataSource(emailService: EmailService) =
    new SpotFXLimMarketDataSource(LIMService.Null, emailService, template)

  protected def expectEmailsForNoData(emailService: EmailService) = {
    oneOf(emailService).send(withArg(equal(template.copy(subject = "No Spot FX for: Bloomberg Generic Rate on 27Oct2011",
      body = expectBodyWith(missing(titanCurrencies - CNY : _*))))))

    oneOf(emailService).send(withArg(equal(template.copy(subject = "No Spot FX for: CFETS on 27Oct2011",
      body = expectBodyWith(missing(CNY))))))
  }

  protected def completeSetOfData = titanCurrencies.map(rateFor(_))

  private def rateFor(ccy: UOM): (TimedMarketDataKey, SpotFXData) = {
    (TimedMarketDataKey(observationDay.atTimeOfDay(ObservationTimeOfDay.LondonClose), SpotFXDataKey(ccy)),
      SpotFXData(Quantity(1.0, ccy)))
  }

  private def ratesForEveryCurrencyExcept(uom: UOM) = (titanCurrencies - uom).map(rateFor(_)).toList
}