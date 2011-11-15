package starling.metals.datasources

import starling.marketdata._
import starling.quantity.Quantity
import org.jmock.Expectations._
import starling.services.EmailService
import starling.utils.ImplicitConversions._
import starling.market.FuturesExchangeFactory._
import starling.market.{Commodity, FuturesExchange, FuturesMarket, Market}
import starling.daterange.TimeZone
import Market._
import collection.immutable.Map
import starling.lim.{LimNode, LIMService}


class PriceLimMarketDataSourceTests extends LimMarketDataSourceTests[PriceLimMarketDataSource] {
  import context._; import expectations._

  "DataSource should send emails when there's missing data for a commodity" in {
    val (dataStore, emailService, dataSource) = create

    expecting {
      val nonLim = LME_STEEL_BILLETS
      val nonBloomberg = COMEX_PALLADIUM
      val nonMetals = futuresMarketFromName("Shanghai Fuel Oil")

      dataStore.queryLatestReturns(pricesForEveryCommodityExcept(
        LME_ALUMINIUM, COMEX_GOLD, SHANGHAI_ZINC, nonLim, nonBloomberg, nonMetals))

      oneOf(emailService).send(withArg(equal(template.copy(subject = "Missing Prices for: LME Metals on 27Oct2011",
        body = expectBodyWith(missing(LME_ALUMINIUM), present(LME_ZINC))))))

      oneOf(emailService).send(withArg(equal(template.copy(subject = "Missing Prices for: COMEX Metals on 27Oct2011",
        body = expectBodyWith(missing(COMEX_GOLD), present(COMEX_SILVER))))))

      oneOf(emailService).send(withArg(equal(template.copy(subject = "Missing Prices for: SHFE Metals on 27Oct2011",
        body = expectBodyWith(missing(SHANGHAI_ZINC), present(SHANGHAI_COPPER))))))
    }

    whenExecuting {
      dataSource.runAvailabiltyTasks
    }
  }

  protected def createDataSource(emailService: EmailService): PriceLimMarketDataSource = {
    new PriceLimMarketDataSource(bloombergImports)(LIMService.Null, emailService, template)
  }

  protected def completeSetOfData = pricesForExchanges(LME, COMEX, SHFE)

  lazy val bloombergMarkets: MultiMap[FuturesExchange, FuturesMarket] = MultiMap(
    LME ->> (LME_ALUMINIUM, LME_ZINC), COMEX ->> (COMEX_GOLD, COMEX_SILVER), SHFE ->> (SHANGHAI_ZINC, SHANGHAI_COPPER))

  lazy val bloombergImports = BloombergImports(PriceLimMarketDataSource.sources.map(source =>
    (source.node.name, bloombergMarkets(source.exchange))).toMap.flatMultiMap { case (limFolder, market) =>
    BloombergImport(123, None, market.limSymbol.map(_.name), Some(limFolder), None, None, true, null, TimeZone.UTC)
  }.toList)

  private implicit def enrichFuturesExchange(exchange: FuturesExchange) = new {
    import LIMService.TopRelation.Trafigura.Bloomberg._
    lazy val limFolders: Map[FuturesExchange, LimNode] = Map(LME → Metals.Lme, COMEX → Futures.Comex, SHFE → Futures.Shfe)
    lazy val limFolder = limFolders(exchange).name
    lazy val limMetalMarkets = exchange.markets.filter(_.limSymbol.isDefined).filter(isMetal).filter(correspondsToBloombergImport)

    private def isMetal(market: FuturesMarket) = Commodity.metalsCommodities.contains(market.commodity)
    private def correspondsToBloombergImport(market: FuturesMarket) = bloombergImports.matches(limFolder, market)
  }

  protected def expectEmailsForNoData(emailService: EmailService) {
    oneOf(emailService).send(withArg(equal(template.copy(subject = "No Prices for: LME Metals on 27Oct2011",
      body = expectBodyWith(missing(LME.limMetalMarkets : _*))))))

    oneOf(emailService).send(withArg(equal(template.copy(subject = "No Prices for: COMEX Metals on 27Oct2011",
      body = expectBodyWith(missing(COMEX.limMetalMarkets : _*))))))

    oneOf(emailService).send(withArg(equal(template.copy(subject = "No Prices for: SHFE Metals on 27Oct2011",
      body = expectBodyWith(missing(SHFE.limMetalMarkets : _*))))))
  }

  private def pricesFor(markets: FuturesMarket*): List[(TimedMarketDataKey, PriceData)] = markets.map { market =>
    (TimedMarketDataKey(observationDay.atTimeOfDay(market.exchange.closeTime), PriceDataKey(market)),
        PriceData(Map(observationDay → Quantity(1, market.priceUOM).pq)))
    }.toList

  private def pricesForExchanges(exchanges: FuturesExchange*): List[(TimedMarketDataKey, PriceData)] = exchanges.flatMap { exchange =>
    pricesFor(exchange.limMetalMarkets : _*)
  }.toList

  private def pricesForEveryCommodityExcept(missing: FuturesMarket*) = missing.toMultiMapWithKeys(_.exchange).flatMap {
    case (exchange, missingMarketsForExchange) => pricesFor((exchange.limMetalMarkets -- missingMarketsForExchange) : _*)
  }.toList

}