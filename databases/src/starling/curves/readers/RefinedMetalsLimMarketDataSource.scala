package starling.curves.readers

import collection.immutable.List

import starling.{LIMConnection, LimNode, LIMServer}
import starling.daterange._
import starling.db.{MarketDataEntry, MarketDataSource}
import starling.market._
import starling.marketdata._
import starling.pivot.MarketValue
import starling.quantity.{Quantity, UOM}
import starling.utils.Log

import LIMServer._
import starling.utils.ClosureUtil._
import starling.utils.ImplicitConversions._
import FuturesExchangeFactory._


case class RefinedMetalsLimMarketDataSource(limServer: LIMServer)
  extends MarketDataSource {

  private val fixingsSources = List(ECBSpotFXFixings, LMEFixings, LIBORFixings, MonthlyFuturesFixings)
  private val fxSources = List(BloombergFXRates)

  def read(day: Day) = {
    Map((day, day, PriceDataType) → getPrices(day),
        (day, day, SpotFXDataType) → getValues(day, day, fxSources),
        (day.startOfFinancialYear, day, PriceFixingsHistoryDataType) → getValues(day.startOfFinancialYear, day, fixingsSources)
    )
  }

  private def getPrices(day: Day) = limServer.query { connection =>
    val limRelations = List(new LMELIMRelation(TopRelation.Trafigura.Bloomberg.Metals.Lme, ObservationTimeOfDay.LMEClose))

    limRelations.flatMap { limRelation => {
      val childRelations = connection.getAllRelChildren(limRelation.parent)

      val data = childRelations.flatMap(limRelation.parse(_)).map { case (childRelation, market, dateRange) => {
        connection.getPrice(childRelation, Level.Close, day).map(price => price.add( (market, dateRange) ))
      }}.somes

      data.groupBy(_._2).toList.map { case (market, prices) =>
        MarketDataEntry(day.atTimeOfDay(limRelation.observationTimeOfDay), PriceDataKey(market),
          PriceData.create(prices.map { case (price, _, day) => day → price}, market.priceUOM))
      }
    } }.info(entries => logCountData(entries, limRelations.map(_.parent.name)))
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
      val prices = source.levels.valuesToMap(level => connection.getPrices(childRelation, level, start, end))

      val groupedPrices = prices.toList.flatMap {
        case (level, prices) => prices.map { case (day, price) => (day, (level, price)) }
      }.groupInto(_._1, _._2)

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
}

trait HierarchicalLimSource extends LimSource {
  val parentNodes: List[LimNode]

  def fixingRelationFrom(childRelation: String): Option[(Relation, String)]

  def relationsFrom(connection: LIMConnection) = connection.getAllRelChildren(parentNodes : _*).flatMap(safeFixingRelationFrom)

  private def safeFixingRelationFrom(childRelation: String) = safely(fixingRelationFrom(childRelation)) match {
    case Left(exception) => { Log.debug("Malformed LIM relation: " + childRelation, exception); None }
    case Right(fixingRelation) => fixingRelation
  }
}

object MonthlyFuturesFixings extends HierarchicalLimSource {
  type Relation = MonthlyFuturesRelation
  case class MonthlyFuturesRelation(market: FuturesMarket, month: Month)

  private val Regex = """TRAF\.(\w+)\.(\w+)_(\w+)""".r

  def fixingRelationFrom(childRelation: String) = (childRelation partialMatch {
    case Regex(exchange, limSymbol, reutersDeliveryMonth) => {
      val optMarket: Option[FuturesMarket] = Market.futuresMarkets.find(market =>
        market.exchange.name == exchangeLookup(exchange) && market.limSymbol.map(_.name) == Some(limSymbol))
      val optMonth: Option[Month] = ReutersDeliveryMonthCodes.parse(reutersDeliveryMonth)

      (optMarket, optMonth) partialMatch {
        case (Some(market), Some(month)) => (MonthlyFuturesRelation(market, month), childRelation)
      }
    }
  }).flatOpt

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

  val parentNodes = List(TopRelation.Trafigura.Bloomberg.Futures.Shfe, TopRelation.Trafigura.Bloomberg.Futures.Comex)
  val levels = List(Level.Close, Level.Settle)

  private def exchangeLookup(exchange: String) = if (exchange == "SHFE") "SFS" else exchange
}

object LMEFixings extends LimSource {
  type Relation = LMEFixingRelation
  case class LMEFixingRelation(ring: ObservationTimeOfDay, market: CommodityMarket, tenor:Tenor)

  private val tenors = List(Tenor.CASH, Tenor(Month, 3), Tenor(Month, 15), Tenor(Month, 27))
  private val rings = List(ObservationTimeOfDay.AMR1, ObservationTimeOfDay.Official, ObservationTimeOfDay.PMR1, ObservationTimeOfDay.Unofficial)

  val levels = List(Level.Ask, Level.Bid)

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

  private def keyGroup(fixing: Prices[LMEFixingRelation]) = {
    (fixing.observationDay.atTimeOfDay(fixing.relation.ring), fixing.relation.market)
  }
}

object BloombergFXRates extends HierarchicalLimSource {
  type Relation = FXRelation
  private val Regex = """TRAF\.BGNL\.(...)(...)""".r

  case class FXRelation(from: UOM, to: UOM) {
    def againstUSD(rate: Double): Option[Quantity] = (from, to) partialMatch {
      case (UOM.USD, _) => Quantity(rate, to / from).invert
      case (_, UOM.USD) => Quantity(rate, to / from)
    }
  }

  def marketDataEntriesFrom(allRates: List[Prices[FXRelation]]) = {
    allRates.flatMapO { rates =>
      rates.relation.againstUSD(rates.priceByLevel(Level.Close)).map(fx =>
        MarketDataEntry(rates.observationDay.atTimeOfDay(ObservationTimeOfDay.LondonClose),
          SpotFXDataKey(fx.denominatorUOM), SpotFXData(fx)))
    }
  }

  def fixingRelationFrom(childRelation: String) = (childRelation partialMatch {
    case Regex(from, to) => (UOM.fromStringOption(from), UOM.fromStringOption(to)) partialMatch {
      case (Some(f), Some(t)) => (FXRelation(f, t), childRelation)
    }
  }).flatOpt

  val parentNodes = List(TopRelation.Trafigura.Bloomberg.Currencies.Composite)
  val levels = List(Level.Close)
}

object ECBSpotFXFixings extends HierarchicalLimSource {
  type Relation = UOM

  private val Regex = """ECB(\w+)""".r
  val parentNodes = List(TopRelation.ForeignExchange.Ecb)
  val levels = List(Level.Spot)

  def fixingRelationFrom(childRelation: String): Option[(UOM, String)] = (childRelation partialMatch {
    case Regex(currency) => UOM.fromStringOption(currency)
  }).flatOpt.optPair(childRelation)

  def marketDataEntriesFrom(prices: List[Prices[UOM]]): List[MarketDataEntry] = {
      prices.map { case Prices(currency, priceByLevel, observationDay) =>
      MarketDataEntry(
        observationDay.atTimeOfDay(ObservationTimeOfDay.ECBPublicationTime),
        PriceFixingsHistoryDataKey(currency.toString, Some("ECB")),
        PriceFixingsHistoryData.create(Level.Spot, StoredFixingPeriod.tenor(Tenor(Day, 1)),
          Quantity(priceByLevel(Level.Spot), currency / UOM.EUR))
      )
    }
  }
}