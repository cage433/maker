package starling.curves.readers

import collection.immutable.List

import starling.{LIMConnection, LimNode, LIMServer}
import starling.daterange._
import starling.db.{MarketDataEntry, MarketDataSource}
import starling.market._
import starling.marketdata._
import starling.pivot.MarketValue
import starling.quantity.{Quantity, UOM}
import starling.utils.{Pattern, Log}

import FuturesExchangeFactory._
import Level._
import LIMServer.TopRelation._
import ObservationTimeOfDay._
import Pattern._
import starling.utils.ImplicitConversions._


case class RefinedMetalsLimMarketDataSource(limServer: LIMServer) extends MarketDataSource {
  private val fixingsSources = PriceFixingsHistoryDataType → (List(LMEFixings, LIBORFixings, BloombergTokyoCompositeFXRates,
    BalticFixings,
    new MonthlyFuturesFixings(Trafigura.Bloomberg.Futures.Shfe, Settle),
    new MonthlyFuturesFixings(Trafigura.Bloomberg.Futures.Comex, Close)) ::: SpotFXFixings.all)
  private val spotFXSources = SpotFXDataType → List(BloombergGenericFXRates, CFETSSpotFXFixings)
  private val priceSources = PriceDataType → List(
    new PriceLimSource(new LMELIMRelation(Trafigura.Bloomberg.Metals.Lme, LMEClose)),
    new PriceLimSource(new MonthlyLIMRelation(Trafigura.Bloomberg.Futures.Comex, COMEXClose)),
    new PriceLimSource(new MonthlyLIMRelation(Trafigura.Bloomberg.Futures.Shfe, SHFEClose))
  )

  override def description = List(fixingsSources, spotFXSources, priceSources).flatMap
    { case (marketDataType, sources) => marketDataType.name.pair(sources.flatMap(_.description)).map("%s → %s" % _) }

  def read(day: Day) = Map(getValuesForType(PriceDataType, day, day, priceSources),
                           getValuesForType(SpotFXDataType, day, day, spotFXSources),
                           getValuesForType(PriceFixingsHistoryDataType, day.startOfFinancialYear, day, fixingsSources))

  private def getValuesForType(m: Any, start: Day, end: Day, sources: (MarketDataType, List[LimSource])) =
    (start, end, sources.head) → sources.tail.flatMap(source => getValues(source, start, end).toList)
      .require(containsDistinctTimedKeys, "concatenated sources: %s, produced duplicate MarketDataKeys: " % sources)

  private def getValues(source: LimSource, start: Day, end: Day): List[MarketDataEntry] = limServer.query { connection =>
    val relations = source.relationsFrom(connection)

    val prices = relations.flatMap { case (fixingRelation, childRelation) => {
      val prices = source.levels.toMapWithValues(level => connection.getPrices(childRelation, level, start, end))

      prices.flipNesting.toList.flatMapO { case (observationDay, pricesForLevel) =>
        if (pricesForLevel.isEmpty) None else Some(Prices(fixingRelation, pricesForLevel, observationDay))
      }
    } }

    source.marketDataEntriesFrom(prices).toList
      .map(_.copy(tag = Some("%s (%s)" % (source.getClass.getSimpleName, source.description.mkString(", ")))))
      .require(containsDistinctTimedKeys, "source: %s produced duplicate MarketDataKeys: " % source)
      .debug(entries => "%s (%s): %s values" % (source.getClass.getSimpleName, source.description.mkString(", "), countData(entries)))
  }

  private def countData(entries: List[MarketDataEntry]) = entries.map(_.data.size.getOrElse(0)).sum
}

case class Prices[Relation](relation: Relation, priceByLevel: Map[Level, Double], observationDay: Day) {
  def atTimeOfDay(observationTimeOfDay: ObservationTimeOfDay) = observationDay.atTimeOfDay(observationTimeOfDay)
  def priceFor(level: Level) = priceByLevel(level)
}

abstract class LimSource(val levels: List[Level]) {
  type Relation
  def relationsFrom(connection: LIMConnection): List[(Relation, String)]
  def marketDataEntriesFrom(fixings: List[Prices[Relation]]): Iterable[MarketDataEntry]
  def description: List[String]
  protected def levelDescription = "(" + levels.map(_.name).mkString(", ") + ")"
  protected def exchangeLookup(exchange: String) = if (exchange == "SHFE") "SFS" else exchange
}

abstract class HierarchicalLimSource(val parentNodes: List[LimNode], levels: List[Level]) extends LimSource(levels) {
  def description = parentNodes.map(node => node.name + " " + levelDescription)
  def relationsFrom(connection: LIMConnection) = connection.getAllRelChildren(parentNodes : _*).flatMap(safeRelationFrom)
  def relationExtractor: Extractor[String, Option[Relation]]

  private def safeRelationFrom(childRelation: String): Option[(Relation, String)] = try {
    relationExtractor.unapply(childRelation).flatOpt.optPair(childRelation)
  } catch { case exception => { Log.debug("Malformed LIM relation: " + childRelation, exception); None } }
}

class PriceLimSource(relations: LIMRelation*) extends LimSource(List(Close)) {
  type Relation = LimPrice
  def description = relations.map(_.node.name).toList.map(name => name + " " + levelDescription)

  def relationsFrom(connection: LIMConnection) = relations.toList.flatMap(relation =>
    connection.getAllRelChildren(relation.node).flatMap(childRelation => relation.parse(childRelation).optPair(childRelation)))

  def marketDataEntriesFrom(allPrices: List[Prices[LimPrice]]) = allPrices.groupBy(group _)
    .map { case ((market, observationPeriod), prices) => MarketDataEntry(observationPeriod, PriceDataKey(market),
        PriceData.create(prices.map(price => price.relation.period → price.priceFor(Close)), market.priceUOM))
    }

  private def group(prices: Prices[LimPrice]) =
    prices.relation.market → prices.atTimeOfDay(prices.relation.observationTimeOfDay)
}

class MonthlyFuturesFixings(parentNodes: List[LimNode], levels: List[Level]) extends HierarchicalLimSource(parentNodes, levels) {
  def this(node: LimNode, level: Level) = this(List(node), List(level))

  type Relation = MonthlyFuturesRelation

  case class MonthlyFuturesRelation(market: FuturesMarket, month: Month)

  def relationExtractor = Extractor.regex("""TRAF\.(\w+)\.(\w+)_(\w+)""") { case List(exchange, limSymbol, deliveryMonth) => {
    val optMarket = Market.fromExchangeAndLimSymbol(exchangeLookup(exchange), limSymbol)
    val optMonth = ReutersDeliveryMonthCodes.parse(deliveryMonth)

    (optMarket, optMonth) partialMatch { case (Some(market), Some(month)) => MonthlyFuturesRelation(market, month) }
  } }

  def marketDataEntriesFrom(fixings: List[Prices[MonthlyFuturesRelation]]) = {
    fixings.groupBy(group).map { case ((market, observationPoint), prices) => {
      val data = prices.flatMap { price => price.priceByLevel.map { case (level, priceAtLevel) =>
        (level, StoredFixingPeriod.dateRange(price.relation.month)) → MarketValue.quantity(priceAtLevel, market.priceUOM)
      } }

      MarketDataEntry(observationPoint, PriceFixingsHistoryDataKey(market), PriceFixingsHistoryData.create(data))
    } }
  }

  private def group(prices: Prices[MonthlyFuturesRelation]) =
    (prices.relation.market, prices.observationDay.atTimeOfDay(prices.relation.market.closeTime))
}

object LMEFixings extends LimSource(List(Ask, Bid)) {
  type Relation = LMEFixingRelation
  def description = List("%s/TRAF.LME.<commodity>.<ring>.<tenor> %s" % (Trafigura.Bloomberg.Currencies.Lme, levelDescription))

  case class LMEFixingRelation(ring: ObservationTimeOfDay, market: CommodityMarket, tenor: Tenor)

  private val tenors = List(Tenor.CASH, Tenor(Month, 3), Tenor(Month, 15), Tenor(Month, 27))
  private val rings = List(AMR1, Official, PMR1, Unofficial)

  def relationsFrom(connection: LIMConnection) = for (market <- LME.markets; ring <- rings; tenor <- tenors) yield {
    (LMEFixingRelation(ring, market, tenor), "TRAF.LME.%S.%S.%s" % (market.commodity, ring, tenor))
  }

  def marketDataEntriesFrom(fixings: List[Prices[LMEFixingRelation]]) = {
    val groupedFixings = fixings.groupInto(keyGroup, fixing => (fixing.relation.tenor, fixing.priceByLevel))

    groupedFixings.map { case ((observationPoint, market), fixingsInGroup) =>
      val data = fixingsInGroup.flatMap { case (tenor, priceByLevel) => priceByLevel.map { case (level, price) =>
        (level, StoredFixingPeriod.tenor(tenor)) → MarketValue.quantity(price, market.priceUOM)
      } }

      MarketDataEntry(observationPoint, PriceFixingsHistoryDataKey(market), PriceFixingsHistoryData.create(data))
    }
  }

  private def keyGroup(fixing: Prices[LMEFixingRelation]) =
    (fixing.observationDay.atTimeOfDay(fixing.relation.ring), fixing.relation.market)
}

object BloombergGenericFXRates extends HierarchicalLimSource(List(Trafigura.Bloomberg.Currencies.Composite), List(Close)) {
  type Relation = FXRelation

  case class FXRelation(from: UOM, to: UOM) {
    def againstUSD(rate: Double): Option[(UOM,Quantity)] = (from, to) partialMatch {
      case (UOM.USD, ccy) => (ccy, Quantity(rate, to / from))
      case (ccy, UOM.USD) => (ccy, Quantity(rate, to / from))
    }
  }

  def relationExtractor = Extractor.regex("""TRAF\.BGNL\.(...)(...)""") {
    case List(UOM.Parse(from), UOM.Parse(to)) => Some(FXRelation(from, to))
  }

  def marketDataEntriesFrom(allRates: List[Prices[FXRelation]]) = allRates.flatMap { rates =>
    rates.relation.againstUSD(rates.priceByLevel(Close)).filterNot(_._1 == UOM.CNY).toList.map{ case (ccy,fx) =>
      MarketDataEntry(rates.observationDay.atTimeOfDay(LondonClose), SpotFXDataKey(ccy), SpotFXData(fx))}
  }
}

class SpotFXFixings(exchange: String, timeOfDay: ObservationTimeOfDay, level: Level, against: UOM, regex: String, nodes: LimNode*)
  extends HierarchicalLimSource(nodes.toList, List(level)) {

  type Relation = UOM
  def relationExtractor = Extractor.regex(regex) { case List(UOM.Parse(currency)) => Some(currency) }

  def marketDataEntriesFrom(prices: List[Prices[UOM]]) = prices.map { case Prices(currency, priceByLevel, observationDay) =>
    MarketDataEntry(observationDay.atTimeOfDay(timeOfDay), key(currency), value(priceByLevel(level), currency))
  }

  protected def key(currency: UOM): MarketDataKey = PriceFixingsHistoryDataKey(currency.toString, Some(exchange))

  protected def value(price: Double, currency: UOM): MarketData = PriceFixingsHistoryData.create(
    level, StoredFixingPeriod.tenor(Tenor.OneDay), (Quantity(price, currency / against)))
}

object SpotFXFixings {
  import UOM._; import Trafigura.Bloomberg._

  val all = List(new SpotFXFixings("ECB", ECBPublicationTime, Spot, EUR, """ECB(\w+)""", ForeignExchange.Ecb),
    new SpotFXFixings("LME", LMEClose, Close, USD, """TRAF\.LME\.(\w+)""", Currencies.LME, Currencies.Lme))
}

object CFETSSpotFXFixings extends SpotFXFixings("SFS", SHFEClose, Close, UOM.USD, """TRAF\.CFETS\.(CNY)""",
  Trafigura.Bloomberg.Currencies.Composite) {

  override protected def key(currency: UOM) = SpotFXDataKey(currency)
  override protected def value(price: Double, currency: UOM) = SpotFXData(Quantity(price, currency / UOM.USD))
}

object BloombergTokyoCompositeFXRates extends HierarchicalLimSource(List(Trafigura.Bloomberg.Currencies.Composite), List(Close)) {
  type Relation = Rate
  case class Rate(currency: UOM, tenor: Tenor)

  def relationExtractor = Extractor.regex("""TRAF.CMPT.(\w+).(\w+)""")
    { case List(UOM.Parse(currency), Tenor.Parse(tenor)) => Some(Rate(currency, tenor)) }

  def marketDataEntriesFrom(fixings: List[Prices[Rate]]) = fixings.groupBy(keyGroup).map {
    case((currency, observationPoint), fixingsForCurrency) => {
      val prices = fixingsForCurrency.map { case (Prices(Rate(_, tenor), priceByLevel, _)) =>
        (Close, StoredFixingPeriod.tenor(tenor)) → MarketValue.quantity(priceByLevel(Close), currency / UOM.USD)
      }

      MarketDataEntry(observationPoint, PriceFixingsHistoryDataKey(currency.toString, Some("CMPT")),
        PriceFixingsHistoryData.create(prices))
    }
  }

  private def keyGroup(fixing: Prices[Rate]) = (fixing.relation.currency, fixing.observationDay.atTimeOfDay(SHFEClose))
}