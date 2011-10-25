package starling.curves.readers

import collection.immutable.List

import starling.daterange._
import starling.marketdata._
import starling.utils.ImplicitConversions._
import starling.{LimNode, LIMConnection, LIMServer}
import starling.utils.Pattern.Extractor
import starling.calendar.BusinessCalendars
import starling.scheduler.{ScheduledTime, TaskDescription}
import starling.db.{MarketDataStore, MarketDataEntry}
import starling.market.{CommodityMarket, Market, FuturesExchangeFactory, Level}
import starling.pivot.PivotQuantity
import Level._
import starling.gui.api.{PriceDataEvent, SnapshotIDLabel, PricingGroup}
import starling.databases._

object PriceLimMarketDataSource extends scalaz.Options {
  import LIMServer.TopRelation._
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

case class PriceLimMarketDataSource(limServer: LIMServer, calendars: BusinessCalendars) extends LimMarketDataSource(limServer) {
  import PriceLimMarketDataSource._
  import ScheduledTime._

  override def description = List(priceSources).flatMap
    { case (marketDataType, sources) => marketDataType.name.pair(sources.flatMap(_.description)).map("%s → %s" % _) }

  def read(day: Day) = log.infoWithTime("Getting data from LIM") {
    Map(getValuesForType(PriceDataType.name, day.startOfFinancialYear, day, priceSources))
  }

  override def eventSources(marketDataStore: MarketDataStore) = List(
    priceDataSource(marketDataStore, List(Market.LME_ALUMINIUM)),
    priceDataSource(marketDataStore, List(Market.COMEX_GOLD)),
    priceDataSource(marketDataStore, List(Market.SHANGHAI_ZINC))
  )

//  val exbxgMetals = new DataFlow(FuturesExchangeFactory.EXBXG, PricingGroup.Metals, Nil, metalsEmail, wuXiEmail, "Excel",
//    "WuXi Metals") with NullMarketDataEventSource

  override def availabilityTasks(marketDataStore: MarketDataStore) = List(
    TaskDescription("Verify Lme Metals Prices Available", daily(calendars.LME, 20 H 00), notImplemented),
    TaskDescription("Verify Comex Metals Prices Available", daily(calendars.COMEX, 15 H 30), notImplemented),
    TaskDescription("Verify Shfe Metals Prices Available", daily(calendars.SFS, 15 H 00), notImplemented)
  )
//      tasks(daily(SFE, 16 H 30), availabilityBroadcaster.verifyPricesAvailable(exbxgMetals), verifyPricesValid(exbxgMetals))


  private def priceDataSource(marketDataStore: MarketDataStore, markets: List[CommodityMarket]) =
    PriceDataEventSource(PricingGroup.Metals, PriceDataFlowDataProvider(marketDataStore, markets))
}

class PriceLimSource(relation: LIMRelation) extends LimSource(List(Close)) {
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
          price.relation.period → (price.priceFor(Close) * limMultiplier)
        }), market.priceUOM))
    }

  private def group(prices: Prices[LimPrice]) =
    prices.relation.market → prices.atTimeOfDay(prices.relation.observationTimeOfDay)
}

case class PriceDataEventSource(pricingGroup: PricingGroup, provider: DataFlowDataProvider[CommodityMarket, DateRange, PivotQuantity])
  extends PricingGroupMarketDataEventSource {

  type Key = CommodityMarket
  type MarketType = DateRange
  type CurveType = PivotQuantity

  protected def marketDataEvent(change: MarketDataChange, market: CommodityMarket, periods: List[DateRange], snapshot: SnapshotIDLabel) = {
    Some(PriceDataEvent(change.observationDay, market.name, periods, snapshot, change.isCorrection))
  }

  protected def marketDataProvider = Some(provider)
}

case class PriceDataFlowDataProvider(marketDataStore: MarketDataStore, markets: List[CommodityMarket])
  extends AbstractDataFlowDataProvider[CommodityMarket, DateRange, PivotQuantity](marketDataStore) {

  val marketDataType = PriceDataType.name
  val marketDataKeys = markets.ifDefined(_.map(PriceDataKey(_).asInstanceOf[MarketDataKey]).toSet)

  def marketDataFor(timedData: List[(TimedMarketDataKey, MarketData)]) = timedData.collect {
    case (TimedMarketDataKey(_, PriceDataKey(market)), PriceData(prices)) => (market, prices)
  }.toMap
}
