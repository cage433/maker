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

case class RefinedMetalsLimMarketDataSource(limServer: LIMServer)
  extends MarketDataSource {

  private val fixingsSources: List[LimSource] = List(ECBSpotFXFixings, LMEFixings, LIBORFixings, MonthlyFuturesFixings)
  private val fxSources = List(BloombergFXRates)
  private val priceRelations = List(new LMELIMRelation(TopRelation.Trafigura.Bloomberg.Metals.Lme, ObservationTimeOfDay.LMEClose))

  override def description = ("Fixing".pair(fixingsSources.flatMap(_.description)) ++
                              "SpotFX".pair(fxSources.flatMap(_.description)) ++
                              "Price".pair(priceRelations.map(_.node.name))).toList.map("%s → %s" % _)

  def read(day: Day) = {
    Map((day, day, PriceDataType) → getPrices(day),
        (day, day, SpotFXDataType) → getValues(day, day, fxSources),
        (day.startOfFinancialYear, day, PriceFixingsHistoryDataType) → getValues(day.startOfFinancialYear, day, fixingsSources)
    )
  }

  private def getPrices(day: Day) = limServer.query { connection =>
    priceRelations.flatMap { priceRelation => {
      val childRelations = connection.getAllRelChildren(priceRelation.node)

      val data = childRelations.flatMap(priceRelation.parse(_)).flatMapO { case (childRelation, market, dateRange) =>
        connection.getPrice(childRelation, Level.Close, day).map(_.add(market, dateRange))
      }

      data.groupBy(_._2).toList.map { case (market, prices) =>
        MarketDataEntry(day.atTimeOfDay(priceRelation.observationTimeOfDay), PriceDataKey(market),
          PriceData.create(prices.map { case (price, _, day) => day → price}, market.priceUOM))
      }
    } }.info(entries => logCountData(entries, priceRelations.map(_.node.name)))
  }

  private def getValues(day: Day, sources: List[LimSource]): List[MarketDataEntry] =
    getValues(day.startOfFinancialYear, day, sources)

  private def getValues(start: Day, end: Day, sources: List[LimSource]): List[MarketDataEntry] = {
    sources.flatMap(source => getValues(source, start, end).toList)
      .require(containsDistinctTimedKeys, "concatenated sources: %s, produced duplicate MarketDataKeys: " % sources)
  }

  private def getValues(source: LimSource, start: Day, end: Day): List[MarketDataEntry] = limServer.query { connection =>
    val relations = source.relationsFrom(connection)

    val prices = relations.flatMap { case (fixingRelation, childRelation) => {
      val prices = source.levels.toMapWithValues(level => connection.getPrices(childRelation, level, start, end))

      val groupedPrices = prices.toList.flatMap {
        case (level, prices) => prices.map { case (day, price) => (day, (level, price)) }
      }.groupInto(_.head, _.tail)

      groupedPrices.toList.flatMapO { case (observationDay, pricesForLevel) => {
        if (pricesForLevel.isEmpty) None else Some(Prices(fixingRelation, pricesForLevel.toMap, observationDay))
      } }
    } }

    source.marketDataEntriesFrom(prices)
      .require(containsDistinctTimedKeys, "source: %s produced duplicate MarketDataKeys: " % source)
      .info(entries => logCountData(entries, relations.map(_.tail)))
  }

  private def logCountData(entries: List[MarketDataEntry], relations: List[String]) =
    "Obtained %d values from: %s..." % (countData(entries), relations.take(5).mkString(", "))

  private def countData(entries: List[MarketDataEntry]) = entries.map(_.data.size.getOrElse(0)).sum
}

case class Prices[Relation](relation: Relation, priceByLevel: Map[Level, Double], observationDay: Day)

trait LimSource {
  type Relation
  val levels: List[Level]
  def relationsFrom(connection: LIMConnection): List[(Relation, String)]
  def marketDataEntriesFrom(fixings: List[Prices[Relation]]): List[MarketDataEntry]
  def description: List[String]
}

trait HierarchicalLimSource extends LimSource {
  val parentNodes: List[LimNode]
  def description = parentNodes.map(_.name)
  def relationsFrom(connection: LIMConnection) = connection.getAllRelChildren(parentNodes : _*).flatMap(safeRelationFrom)
  def relationExtractor: Extractor[String, Option[Relation]]

  private def safeRelationFrom(childRelation: String) = safely(relationExtractor.unapply(childRelation).flatOpt) match {
    case Left(exception) => { Log.debug("Malformed LIM relation: " + childRelation, exception); None }
    case Right(fixingRelation) => fixingRelation.optPair(childRelation)
  }
}

object MonthlyFuturesFixings extends HierarchicalLimSource {
  type Relation = MonthlyFuturesRelation
  val parentNodes = List(TopRelation.Trafigura.Bloomberg.Futures.Shfe, TopRelation.Trafigura.Bloomberg.Futures.Comex)
  val levels = List(Level.Close, Level.Settle)

  case class MonthlyFuturesRelation(market: FuturesMarket, month: Month)

  def relationExtractor = Extractor.regex("""TRAF\.(\w+)\.(\w+)_(\w+)""") { case List(exchange, limSymbol, deliveryMonth) => {
    val optMarket: Option[FuturesMarket] = Market.futuresMarkets.find(market =>
      market.exchange.name == exchangeLookup(exchange) && market.limSymbol.map(_.name) == Some(limSymbol))
    val optMonth: Option[Month] = ReutersDeliveryMonthCodes.parse(deliveryMonth)

    (optMarket, optMonth) partialMatch { case (Some(market), Some(month)) => MonthlyFuturesRelation(market, month) }
  } }

  def marketDataEntriesFrom(fixings: List[Prices[MonthlyFuturesRelation]]) = {
    fixings.groupBy(f => (f.relation.market, f.observationDay)).toList.map { case ((market, observationDay), prices) => {
      val data = prices.flatMap { price =>
        price.priceByLevel.toList.map { case (level, priceAtLevel) =>
          (level, StoredFixingPeriod.dateRange(price.relation.month)) → MarketValue.quantity(priceAtLevel, market.priceUOM)
        }
      }.toMap

      MarketDataEntry(observationDay.atTimeOfDay(market.closeTime), PriceFixingsHistoryDataKey(market),
        PriceFixingsHistoryData.create(data))
    } }
  }

  private def exchangeLookup(exchange: String) = if (exchange == "SHFE") "SFS" else exchange
}

object LMEFixings extends LimSource {
  type Relation = LMEFixingRelation
  def description = List("TRAF.LME.<commodity>.<ring>.<tenor>")
  val levels = List(Level.Ask, Level.Bid)

  case class LMEFixingRelation(ring: ObservationTimeOfDay, market: CommodityMarket, tenor:Tenor)

  private val tenors = List(Tenor.CASH, Tenor(Month, 3), Tenor(Month, 15), Tenor(Month, 27))
  private val rings = List(ObservationTimeOfDay.AMR1, ObservationTimeOfDay.Official, ObservationTimeOfDay.PMR1, ObservationTimeOfDay.Unofficial)

  def relationsFrom(connection: LIMConnection) = for (market <- LME.markets; ring <- rings; tenor <- tenors) yield {
    (LMEFixingRelation(ring, market, tenor), "TRAF.LME.%s.%s.%s" % (market.commodity.name.toUpperCase, ring.name.toUpperCase, tenor.toString))
  }

  def marketDataEntriesFrom(fixings: List[Prices[LMEFixingRelation]]): List[MarketDataEntry] = {
    val groupedFixings = fixings.groupInto(keyGroup, fixing => (fixing.relation.tenor, fixing.priceByLevel))

    groupedFixings.map { case ((observationPoint, market), fixingsInGroup) =>
      val data = fixingsInGroup.flatMap { case (tenor, priceByLevel) =>
        priceByLevel.toList.map { case (level, price) =>
          (level, StoredFixingPeriod.tenor(tenor)) → MarketValue.quantity(price, market.priceUOM)
        }
      }.toMap

      MarketDataEntry(observationPoint, PriceFixingsHistoryDataKey(market), PriceFixingsHistoryData.create(data))
    }.toList
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

object ECBSpotFXFixings extends HierarchicalLimSource {
  type Relation = UOM
  val parentNodes = List(TopRelation.ForeignExchange.Ecb)
  val levels = List(Level.Spot)

  def relationExtractor = Extractor.regex("""ECB(\w+)""") { case List(currency) => UOM.fromStringOption(currency) }

  def marketDataEntriesFrom(prices: List[Prices[UOM]]) = prices.map { case Prices(currency, priceByLevel, observationDay) =>
    MarketDataEntry(observationDay.atTimeOfDay(ObservationTimeOfDay.ECBPublicationTime),
      PriceFixingsHistoryDataKey(currency.toString, Some("ECB")),
      PriceFixingsHistoryData.create(Level.Spot, StoredFixingPeriod.tenor(Tenor(Day, 1)),
        Quantity(priceByLevel(Level.Spot), currency / UOM.EUR)))
  }
}