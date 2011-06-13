package starling.curves.readers

import collection.immutable.List

import starling.{LIMConnection, LimNode, LIMServer}
import starling.daterange._
import starling.db.{MarketDataEntry, MarketDataSource}
import starling.market._
import starling.marketdata._
import starling.pivot.MarketValue
import starling.quantity.{Quantity, UOM}
import LIMServer._
import starling.utils.ClosureUtil._
import starling.utils.ImplicitConversions._
import FuturesExchangeFactory._
import starling.utils.{Pattern, Log}
import Pattern._

case class RefinedMetalsLimMarketDataSource(limServer: LIMServer) extends MarketDataSource {
  private val fixingsSources = PriceFixingsHistoryDataType → List(
    ECBSpotFXFixings, LMESpotFXFixings, CFETSSpotFXFixings, LMEFixings, LIBORFixings,
    new MonthlyFuturesFixings(TopRelation.Trafigura.Bloomberg.Futures.Shfe, Level.Settle),
    new MonthlyFuturesFixings(TopRelation.Trafigura.Bloomberg.Futures.Comex, Level.Close))
  private val spotFXSources = SpotFXDataType → List(BloombergFXRates)
  private val priceSources = PriceDataType → List(new PriceLimSource(
    new LMELIMRelation(TopRelation.Trafigura.Bloomberg.Metals.Lme, ObservationTimeOfDay.LMEClose),
    new MonthlyLIMRelation(TopRelation.Trafigura.Bloomberg.Futures.Comex, ObservationTimeOfDay.COMEXClose),
    new MonthlyLIMRelation(TopRelation.Trafigura.Bloomberg.Futures.Shfe, ObservationTimeOfDay.SHFEClose)
  ))

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
      .require(containsDistinctTimedKeys, "source: %s produced duplicate MarketDataKeys: " % source)
      .info(entries => "%s (%s): %s values" % (source.getClass.getSimpleName, source.description.mkString(", "), countData(entries)))
  }

  private def countData(entries: List[MarketDataEntry]) = entries.map(_.data.size.getOrElse(0)).sum
}

case class Prices[Relation](relation: Relation, priceByLevel: Map[Level, Double], observationDay: Day) {
  def atTimeOfDay(observationTimeOfDay: ObservationTimeOfDay) = observationDay.atTimeOfDay(observationTimeOfDay)
  def priceFor(level: Level) = priceByLevel(level)
}

trait LimSource {
  type Relation
  val levels: List[Level]
  def relationsFrom(connection: LIMConnection): List[(Relation, String)]
  def marketDataEntriesFrom(fixings: List[Prices[Relation]]): Iterable[MarketDataEntry]
  def description: List[String]
  protected def levelDescription = "(" + levels.map(_.name).mkString(", ") + ")"
}

trait HierarchicalLimSource extends LimSource {
  val parentNodes: List[LimNode]
  def description = parentNodes.map(node => node.name + " " + levelDescription)
  def relationsFrom(connection: LIMConnection) = connection.getAllRelChildren(parentNodes : _*).flatMap(safeRelationFrom)
  def relationExtractor: Extractor[String, Option[Relation]]

  private def safeRelationFrom(childRelation: String) = safely(relationExtractor.unapply(childRelation).flatOpt) match {
    case Left(exception) => { Log.debug("Malformed LIM relation: " + childRelation, exception); None }
    case Right(fixingRelation) => fixingRelation.optPair(childRelation)
  }
}

class PriceLimSource(relations: LIMRelation*) extends LimSource {
  val levels = List(Level.Close)
  type Relation = LimPrice
  def description = relations.map(_.node.name).toList.map(name => name + " " + levelDescription)

  def relationsFrom(connection: LIMConnection) = relations.toList.flatMap(relation =>
    connection.getAllRelChildren(relation.node).flatMap(childRelation => relation.parse(childRelation).optPair(childRelation)))

  def marketDataEntriesFrom(allPrices: List[Prices[LimPrice]]) = allPrices.groupBy(group _)
    .map { case ((market, observationPeriod), prices) => MarketDataEntry(observationPeriod, PriceDataKey(market),
        PriceData.create(prices.map(price => price.relation.period → price.priceFor(Level.Close)), market.priceUOM))
    }

  private def group(prices: Prices[LimPrice]) =
    prices.relation.market → prices.atTimeOfDay(prices.relation.observationTimeOfDay)
}

class MonthlyFuturesFixings(val parentNodes: List[LimNode], val levels: List[Level]) extends HierarchicalLimSource {
  def this(node: LimNode, level: Level) = this(List(node), List(level))

  type Relation = MonthlyFuturesRelation

  case class MonthlyFuturesRelation(market: FuturesMarket, month: Month)

  def relationExtractor = Extractor.regex("""TRAF\.(\w+)\.(\w+)_(\w+)""") { case List(exchange, limSymbol, deliveryMonth) => {
    val optMarket = FuturesMarket.fromExchangeAndLimSymbol(exchangeLookup(exchange), limSymbol)
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

  private def exchangeLookup(exchange: String) = if (exchange == "SHFE") "SFS" else exchange
  private def group(prices: Prices[MonthlyFuturesRelation]) =
    (prices.relation.market, prices.observationDay.atTimeOfDay(prices.relation.market.closeTime))
}

object LMEFixings extends LimSource {
  type Relation = LMEFixingRelation
  def description = List("TRAF.LME.<commodity>.<ring>.<tenor>" + " " + levelDescription)
  val levels = List(Level.Ask, Level.Bid)

  case class LMEFixingRelation(ring: ObservationTimeOfDay, market: CommodityMarket, tenor:Tenor)

  private val tenors = List(Tenor.CASH, Tenor(Month, 3), Tenor(Month, 15), Tenor(Month, 27))
  private val rings = List(ObservationTimeOfDay.AMR1, ObservationTimeOfDay.Official, ObservationTimeOfDay.PMR1, ObservationTimeOfDay.Unofficial)

  def relationsFrom(connection: LIMConnection) = for (market <- LME.markets; ring <- rings; tenor <- tenors) yield {
    (LMEFixingRelation(ring, market, tenor), "TRAF.LME.%s.%s.%s" % (market.commodity.name.toUpperCase, ring.name.toUpperCase, tenor.toString))
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

object BloombergFXRates extends HierarchicalLimSource {
  type Relation = FXRelation
  val parentNodes = List(TopRelation.Trafigura.Bloomberg.Currencies.Composite)
  val levels = List(Level.Close)

  case class FXRelation(from: UOM, to: UOM) {
    def againstUSD(rate: Double): Option[Quantity] = (from, to) partialMatch {
      case (UOM.USD, _) => Quantity(rate, to / from).invert
      case (_, UOM.USD) => Quantity(rate, to / from)
    }
  }

  def relationExtractor = Extractor.regex("""TRAF\.BGNL\.(...)(...)""") { case List(from, to) =>
    (UOM.fromStringOption(from), UOM.fromStringOption(to)) partialMatch { case (Some(f), Some(t)) => FXRelation(f, t) }
  }

  def marketDataEntriesFrom(allRates: List[Prices[FXRelation]]) = allRates.flatMapO { rates =>
    rates.relation.againstUSD(rates.priceByLevel(Level.Close)).map(fx =>
      MarketDataEntry(rates.observationDay.atTimeOfDay(ObservationTimeOfDay.LondonClose),
        SpotFXDataKey(fx.denominatorUOM), SpotFXData(fx)))
  }
}

abstract class SpotFXFixings(exchange: String, timeOfDay: ObservationTimeOfDay, level: Level, currency: UOM, nodes: LimNode*)
  extends HierarchicalLimSource {

  type Relation = UOM
  val parentNodes = nodes.toList
  val levels = List(level)

  def marketDataEntriesFrom(prices: List[Prices[UOM]]) = prices.map { case Prices(currency, priceByLevel, observationDay) =>
    MarketDataEntry(observationDay.atTimeOfDay(timeOfDay),
      PriceFixingsHistoryDataKey(currency.toString, Some(exchange)),
      PriceFixingsHistoryData.create(level, StoredFixingPeriod.tenor(Tenor(Day, 1)),
        Quantity(priceByLevel(level), currency / currency)))
  }
}

object ECBSpotFXFixings extends SpotFXFixings("ECB", ObservationTimeOfDay.ECBPublicationTime, Level.Spot, UOM.EUR,
  TopRelation.ForeignExchange.Ecb) {

  def relationExtractor = Extractor.regex("""ECB(\w+)""") { case List(currency) => UOM.fromStringOption(currency) }
}

object LMESpotFXFixings extends SpotFXFixings("LME", ObservationTimeOfDay.LMEClose, Level.Close, UOM.USD,
  TopRelation.Trafigura.Bloomberg.Currencies.LME, TopRelation.Trafigura.Bloomberg.Currencies.Lme) {

  def relationExtractor = Extractor.regex("""TRAF\.LME\.(\w+)""") { case List(currency) => UOM.fromStringOption(currency) }
}

object CFETSSpotFXFixings extends SpotFXFixings("SFS", ObservationTimeOfDay.SHFEClose, Level.Close, UOM.USD,
  TopRelation.Trafigura.Bloomberg.Currencies.Composite) {

  def relationExtractor = Extractor.regex("""TRAF\.CFETS\.(\w+)""") { case List(currency) => UOM.fromStringOption(currency) }
}