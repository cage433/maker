package starling.metals.datasources


import starling.daterange._
import starling.marketdata._
import starling.utils.ImplicitConversions._
import starling.utils.Pattern.Extractor
import starling.db.{MarketDataStore, MarketDataEntry}
import starling.pivot.PivotQuantity
import starling.databases._
import starling.market._
import starling.gui.api._
import scalaz.Scalaz._
import starling.scheduler.{ScheduledTask, ScheduledTime, TaskDescription}
import starling.lim.{LIMService, LimNode, LIMConnection}
import starling.services.EmailService
import starling.curves.readers.VerifyMarketDataAvailable
import com.lim.mimapi.RelType
import java.lang.String
import scalaz.Scalaz._
import collection.immutable.{Map, List}

object PriceLimMarketDataSource extends scalaz.Options {
  import LIMService.TopRelation.Trafigura._

  val sources = List(new LMEPriceLimSource,
    new OldPriceLimSource(new MonthlyLIMRelation(Bloomberg.Futures.Comex, FuturesExchangeFactory.COMEX)),
    new OldPriceLimSource(new MonthlyLIMRelation(Bloomberg.Futures.Shfe, FuturesExchangeFactory.SHFE)))

  class MonthlyLIMRelation(val node: LimNode, override val exchange: FuturesExchange, prefix: String,
    override val relTypes: Set[RelType]) extends LIMRelation {

    def this(node: LimNode, exchange: FuturesExchange) = this(node, exchange, "TRAF.", Set(RelType.CATEGORY))

    val extractor = Extractor.regex[Option[LimPrice]](prefix + """%s\.([^_]+)_(\d\d\d\d\D)""" % exchange.name) {
      case List(lim, deliveryMonth) =>

      val optMarket = exchange.markets.find(_.limSymbol.map(_.name) == Some(lim))
      val optMonth = ReutersDeliveryMonthCodes.parse(deliveryMonth)

      (optMarket, optMonth) match {
        case (Some(market), Some(month)) => Some(LimPrice(market, month, exchange.closeTime))
        case (None, _) => debug("No Market with exchange: %s and LIM Symbol: %s" % (exchange.name, lim))
        case (_, None) => debug("Cannot parse Reuters delivery month: " + deliveryMonth)
      }
    }
  }
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

abstract class PriceLimSource extends LimSource(List(Level.Close)) {
  val exchange: FuturesExchange
  val node: LimNode
}

class OldPriceLimSource(relation: LIMRelation) extends PriceLimSource {
  val (node, nodes, exchange) = (relation.node, List(relation.node), relation.exchange)
  type Relation = LimPrice
  def description = nodes.map(_.name + " " + levelDescription)

  override def relationsFrom(connection: LIMConnection) = connection.getAllRelChildren(List(relation.node), relation.relTypes)
    .flatMap(childRelation => relation.parse(childRelation).optPair(childRelation))

  override def marketDataEntriesFrom(allPrices: List[Prices[LimPrice]]) = allPrices.groupBy(group _)
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

class LMEPriceLimSource extends PriceLimSource {
  val exchange = FuturesExchangeFactory.LME
  val node = LIMService.TopRelation.Trafigura.Bloomberg.Metals.Lme
  type Relation = Nothing
  def description = List(node.name + " TRAF.LME.* (Close)")

  case class LMEPrice(market: FuturesMarket, day: Day) {
    def priceFor(price: Double): (DateRange, Double) = day → (price * market.limSymbol.fold(_.multiplier, 1.0))

    override def toString = "TRAF.LME.%S.%s" % (market.commodity.limName, day.toString("yyyy.MM.dd"))
  }

  override def marketDataEntriesFrom(connection: LIMConnection, start: Day, end: Day) = {
    val futureDays: List[Day] = {
      val startMonth = start.containingMonth
      val (days, wednesdays, thirdWednesdays) = (
        (start upto (start.add(3, Month) + 2)).days.filter(_.isWeekday),
        (startMonth upto (startMonth + 6)).flatMap(_.daysMatching(DayOfWeek.wednesday)),
        (startMonth upto (startMonth + (10 * 12 + 3))).map(_.thirdWednesday)
      )

      days ++ wednesdays ++ thirdWednesdays
    }.toSet.toList.sortWith(_ < _)

    def relationsFor(market: FuturesMarket): List[LMEPrice] = futureDays.map(LMEPrice(market, _)).toList

    val pricesByMarket = FuturesExchangeFactory.LME.markets.toMapWithValues { market =>
      connection.typedPrices(relationsFor(market), Level.Close, start, end)
    }

    pricesByMarket.mapNested { case (market, observationDay, prices) =>
      MarketDataEntry(observationDay.atTimeOfDay(FuturesExchangeFactory.LME.closeTime), PriceDataKey(market),
        PriceData.create(prices.map { case (relation, price) => relation.priceFor(price) }, market.priceUOM))
    }.toList
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