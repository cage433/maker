package starling.metals.datasources


import starling.daterange._
import starling.marketdata._
import starling.utils.ImplicitConversions._
import starling.db.MarketDataStore
import starling.pivot.PivotQuantity
import starling.databases._
import starling.market._
import starling.gui.api._
import starling.scheduler.{ScheduledTask, ScheduledTime, TaskDescription}
import starling.lim.{LIMService, LimNode}
import starling.services.EmailService
import starling.curves.readers.VerifyMarketDataAvailable
import java.lang.String
import scalaz.Scalaz._
import collection.immutable.{Map, List}

object PriceLimMarketDataSource extends scalaz.Options {
  import LIMService.TopRelation.Trafigura._

  val sources = List(new LMEPriceLimSource,
    new MonthlyPriceLimSource(Bloomberg.Futures.Comex, FuturesExchangeFactory.COMEX),
    new MonthlyPriceLimSource(Bloomberg.Futures.Shfe, FuturesExchangeFactory.SHFE))
}

case class PriceLimMarketDataSource(bloombergImports: BloombergImports)(service: LIMService, emailService: EmailService, template: Email)
  extends LimMarketDataSource(service, PriceDataType.name) {

  import PriceLimMarketDataSource._
  import FuturesExchangeFactory._

  override def description = descriptionFor(sources)
  def read(day: Day) = Map(getValuesForType(earliestDayToImport(day), day, sources))

  override def eventSources(marketDataStore: MarketDataStore) = limMetalMarketsForExchanges.map(markets =>
    PriceDataEventSource(PricingGroup.Metals, PriceMarketDataProvider(marketDataStore, markets)))

  override def availabilityTasks(marketDataStore: MarketDataStore) = List(
    task("LME Metals", limDaily(LME.calendar, 20 H 00), lme, marketDataStore),
    task("COMEX Metals", limDaily(COMEX.calendar, 18 H 00), comex, marketDataStore),
    task("SHFE Metals", limDaily(SHFE.calendar, 16 H 02), shfe, marketDataStore)
  )

//  val exbxgMetals = new DataFlow(FuturesExchangeFactory.EXBXG, PricingGroup.Metals, Nil, metalsEmail, wuXiEmail, "Excel",
//    "WuXi Metals") with NullMarketDataEventSource
//      tasks(daily(SFE, 16 H 30), availabilityBroadcaster.verifyPricesAvailable(exbxgMetals), verifyPricesValid(exbxgMetals))

  private lazy val limMetalMarketsForExchanges@List(lme, comex, shfe) = sources.map(pricesSource => {
    pricesSource.exchange.markets.filter(_.limSymbol.isDefined).filter(isMetal).filter(correspondsToBloombergImport(pricesSource.node))
  } )

  //require(lme != Nil, "LME"); require(comex != Nil, "COMEX"); require(shfe != Nil, "SHFE")

  private def isMetal(market: FuturesMarket) = Commodity.metalsCommodities.contains(market.commodity)
  private def correspondsToBloombergImport(node: LimNode)(market: FuturesMarket) = bloombergImports.matches(node.name, market)

  private def task(name: String, time: ScheduledTime, markets: List[CommodityMarket], marketDataStore: MarketDataStore): TaskDescription =
    TaskDescription("Verify %s Prices Available" % name, time, task(name, marketDataStore, markets))

  private def task(name: String, marketDataStore: MarketDataStore, markets: List[CommodityMarket]): ScheduledTask =
    new VerifyMarketDataAvailable(marketDataStore, MarketDataSelection(Some(PricingGroup.Metals)), PriceDataType.name,
      markets.map(PriceDataKey(_)).toSet, emailService, template) {

      override protected def emailFor(observationDay: Day) = queryLatest(observationDay) |> { _ match {
        case Nil => Some(template.copy(subject = "No Prices for: %s on %s" % (name, observationDay),
                                       body = markets.map("MISSING: " + _).sorted.mkHtml()))
        case prices => {
          val marketAvailability = {
            val availableMarkets = prices.map(_._1.key.asInstanceOf[PriceDataKey].market).toSet

            markets.groupBy(availableMarkets.contains(_) ? "Present" | "MISSING")
          }

          marketAvailability.contains("MISSING").option {
            template.copy(subject = "Missing Prices for: %s on %s" % (name, observationDay),
                          body = marketAvailability.flatMultiMap { _.format("%s: %s") }.toList.sorted.mkHtml())
          }
        }
      } }
    }
}

case class PriceDataEventSource(pricingGroup: PricingGroup, provider: MarketDataProvider[CommodityMarket, DateRange, PivotQuantity])
  extends PricingGroupMarketDataEventSource {

  type Key = CommodityMarket
  type MarketType = DateRange
  type CurveType = PivotQuantity

  protected def marketDataEvent(change: MarketDataChange, market: CommodityMarket, periods: List[DateRange], snapshot: SnapshotIDLabel) = {
    Some(PriceDataEvent(change.observationDay, market.name, periods, snapshot, change.isCorrection))
  }

  protected def marketDataProvider = Some(provider)
}

case class PriceMarketDataProvider(marketDataStore: MarketDataStore, markets: List[CommodityMarket])
  extends AbstractMarketDataProvider[CommodityMarket, DateRange, PivotQuantity](marketDataStore) {

  val marketDataType = PriceDataType.name
  val marketDataKeys = markets.ifDefined(_.map(PriceDataKey(_).asInstanceOf[MarketDataKey]).toSet)

  def marketDataFor(timedData: List[(TimedMarketDataKey, MarketData)]) = timedData.collect {
    case (TimedMarketDataKey(_, PriceDataKey(market)), PriceData(prices)) => (market, prices)
  }.toMap
}