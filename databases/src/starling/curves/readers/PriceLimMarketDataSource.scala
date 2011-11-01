package starling.curves.readers

import collection.immutable.List

import starling.daterange._
import starling.marketdata._
import starling.utils.ImplicitConversions._
import starling.utils.Pattern.Extractor
import starling.db.{MarketDataStore, MarketDataEntry}
import starling.pivot.PivotQuantity
import starling.databases._
import starling.market._
import starling.utils.Broadcaster
import starling.gui.api._
import scalaz.Scalaz._
import starling.scheduler.{ScheduledTask, ScheduledTime, TaskDescription}
import starling.lim.{LIMService, LimNode, LIMConnection}
import starling.calendar.{BusinessCalendars}

object PriceLimMarketDataSource extends scalaz.Options {
  import LIMService.TopRelation._
  import ObservationTimeOfDay._

  val priceSources = PriceDataType.name → List(
    new PriceLimSource(new LMELIMRelation(Trafigura.Bloomberg.Metals.Lme, LMEClose)),
    new PriceLimSource(new MonthlyLIMRelation(Trafigura.Bloomberg.Futures.Comex, COMEXClose)),
    new PriceLimSource(new MonthlyLIMRelation(Trafigura.Bloomberg.Futures.Shfe, SHFEClose)))

  class LMELIMRelation(val node: LimNode, observationTimeOfDay: ObservationTimeOfDay) extends LIMRelation {
    private lazy val lmeMarkets = FuturesExchangeFactory.LME.marketsByCommodityName + "steelbillet" → Market.LME_STEEL_BILLETS

    protected val extractor = Extractor.regex[Option[LimPrice]]("""TRAF\.LME\.(\w+)\.(\d+)\.(\d+)\.(\d+)""") {
      case List(commodity, y, m, d) => some(LimPrice(lmeMarkets(commodity.toLowerCase), Day(y, m, d), observationTimeOfDay))
    }
  }

  class MonthlyLIMRelation(val node: LimNode, observationTimeOfDay: ObservationTimeOfDay) extends LIMRelation {
    protected val extractor = Extractor.regex[Option[LimPrice]]("""TRAF\.(\w+)\.(\w+)_(\w+)""") {
      case List(exchange, lim, deliveryMonth) =>
      val optMarket = Market.fromExchangeAndLimSymbol(exchangeLookup(exchange), lim)
      val optMonth = ReutersDeliveryMonthCodes.parse(deliveryMonth)

      (optMarket, optMonth) match {
        case (Some(market), Some(month)) => Some(LimPrice(market, month, observationTimeOfDay))
        case (None, _) => debug("No Market with exchange: %s and LIM Symbol: %s" % (exchange, lim))
        case (_, None) => debug("Cannot parse Reuters delivery month: " + deliveryMonth)
      }
    }

    def exchangeLookup(exchange: String) = if (exchange == "SHFE") "SFS" else exchange
  }
}

case class PriceLimMarketDataSource(service: LIMService, calendars: BusinessCalendars, broadcaster: Broadcaster,
  sender: String, recipient: String) extends LimMarketDataSource(service) {

  import PriceLimMarketDataSource._
  import ScheduledTime._
  import FuturesExchangeFactory._

  override def description = List(priceSources).flatMap
    { case (marketDataType, sources) => marketDataType.name.pair(sources.flatMap(_.description)).map("%s → %s" % _) }

  def read(day: Day) = log.infoWithTime("Getting data from LIM") {
    Map(getValuesForType(PriceDataType.name, day.startOfFinancialYear, day, priceSources))
  }

  override def eventSources(marketDataStore: MarketDataStore) = marketsForExchanges.map(markets =>
    PriceDataEventSource(PricingGroup.Metals, PriceMarketDataProvider(marketDataStore, markets)))

  override def availabilityTasks(marketDataStore: MarketDataStore) = List(
    task("LME Metals", daily(calendars.LME, 20 H 00), lme, marketDataStore),
    task("COMEX Metals", daily(calendars.COMEX, 15 H 30), comex, marketDataStore),
    task("SHFE Metals", daily(calendars.SFS, 15 H 00), shfe, marketDataStore)
  )

//  val exbxgMetals = new DataFlow(FuturesExchangeFactory.EXBXG, PricingGroup.Metals, Nil, metalsEmail, wuXiEmail, "Excel",
//    "WuXi Metals") with NullMarketDataEventSource
//      tasks(daily(SFE, 16 H 30), availabilityBroadcaster.verifyPricesAvailable(exbxgMetals), verifyPricesValid(exbxgMetals))

  private val marketsForExchanges@List(lme, comex, shfe) =
    List(LME, COMEX, SFS).map(exchange => Market.marketsWithExchange(exchange).filter(_.limSymbol.isDefined))

  private def task(name: String, time: ScheduledTime, markets: List[CommodityMarket], marketDataStore: MarketDataStore): TaskDescription =
    TaskDescription("Verify %s Prices Available" % name, time, task(name, marketDataStore, markets))

  private def task(name: String, marketDataStore: MarketDataStore, markets: List[CommodityMarket]): ScheduledTask =
    new VerifyAnyMarketDataAvailable(name, marketDataStore, MarketDataSelection(Some(PricingGroup.Metals)), PriceDataType.name,
      markets.map(PriceDataKey(_)).toSet, broadcaster, sender, recipient)
}

class PriceLimSource(relation: LIMRelation) extends LimSource(List(Level.Close)) {
  val nodes = List(relation.node)
  type Relation = LimPrice
  def description = nodes.map(_.name + " " + levelDescription)

  def relationsFrom(connection: LIMConnection) =
    connection.getAllRelChildren(relation.node).flatMap(childRelation => relation.parse(childRelation).optPair(childRelation))

  def marketDataEntriesFrom(allPrices: List[Prices[LimPrice]]) = allPrices.groupBy(group _)
    .map { case ((market, observationPeriod), prices) => MarketDataEntry(observationPeriod, PriceDataKey(market),
        PriceData.create(prices.map(price => {
          val limMultiplier = market.limSymbol match {
            case Some(ls) => ls.multiplier
            case None => 1.0
          }
          price.relation.period → (price.priceFor(Level.Close) * limMultiplier)
        }), market.priceUOM))
    }

  private def group(prices: Prices[LimPrice]) =
    prices.relation.market → prices.atTimeOfDay(prices.relation.observationTimeOfDay)
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