package starling.curves.readers

import starling.market._
import starling.marketdata._
import starling.db.{MarketDataEntry, MarketDataSource}
import starling.utils.ImplicitConversions._
import starling.daterange._
import collection.immutable.List
import starling.quantity.{Quantity, UOM}
import starling.{LIMConnection, LimNode, LIMServer}
import LIMServer._
import starling.utils.Log
import starling.pivot.MarketValue
import starling.utils.ClosureUtil._
import starling.utils.ImplicitConversions._
import collection.mutable.ListBuffer
import starling.concurrent.MP._
import UOM._
import starling.calendar.{BusinessCalendarSet, HolidayTablesFactory, BusinessCalendar}
import Day._


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
    } }
  }

  private def getValues(day: Day, sources: List[LimSource]): List[MarketDataEntry] =
    getValues(day.startOfFinancialYear, day, sources)

  private def getValues(start: Day, end: Day, sources: List[LimSource]): List[MarketDataEntry] = {
    sources.flatMap(source => getValues(source, start, end).toList)
      .require(containsDistinctTimedKeys, "concatenated sources: %s, produced duplicate MarketDataKeys: " % sources)
  }

  private def getValues(source: LimSource, start: Day, end: Day): List[MarketDataEntry] = limServer.query { connection =>
    val prices = source.relationsFrom(connection).flatMap { case (fixingRelation, childRelation) => {
      val prices = source.levels.valuesToMap(level => connection.getPrices(childRelation, level, start, end))

      val groupedPrices = prices.toList.flatMap {
        case (level, prices) => prices.map { case (day, price) => (day, (level, price)) }
      }.groupInto(_._1, _._2)

      groupedPrices.toList.flatMapO { case (observationDay, pricesForLevel) => {
        if (pricesForLevel.isEmpty) None else Some(Prices(fixingRelation, pricesForLevel.toMap, observationDay))
      } }
    }}

    source.marketDataEntriesFrom(prices)
      .require(containsDistinctTimedKeys, "source: %s produced duplicate MarketDataKeys: " % source)
  }
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

  def relationsFrom(connection: LIMConnection) = {
    connection.getAllRelChildren(parentNodes : _*).flatMap(safeFixingRelationFrom)
  }

  private def safeFixingRelationFrom(childRelation: String) = safely(fixingRelationFrom(childRelation)) match {
    case Left(exception) => { Log.debug("Malformed LIM relation: " + childRelation, exception); None }
    case Right(fixingRelation) => fixingRelation
  }
}

object MonthlyFuturesFixings extends HierarchicalLimSource {
  type Relation = SHFERelation
  case class SHFERelation(market: FuturesMarket, month: Month)

  private val Regex = """TRAF\.(\w+)\.(\w+)_(\w+)""".r

  def fixingRelationFrom(childRelation: String) = (childRelation partialMatch {
    case Regex(exchange, limSymbol, reutersDeliveryMonth) => {
      val optMarket: Option[FuturesMarket] = Market.futuresMarkets.find(market => market.limSymbol.map(_.name) == Some(limSymbol))
      val optMonth: Option[Month] = ReutersDeliveryMonthCodes.parse(reutersDeliveryMonth)

      (optMarket, optMonth) partialMatch {
        case (Some(market), Some(month)) => (SHFERelation(market, month), childRelation)
      }
    }
  }).flatOpt

  def marketDataEntriesFrom(fixings: List[Prices[SHFERelation]]) = {
    fixings.groupBy(f => (f.relation.market, f.observationDay)).toList.map { case ((market, observationDay), prices) => {
        val data = prices.flatMap { price =>
          price.priceByLevel.toList.map { case (level, priceAtLevel) =>
            (level, StoredFixingPeriod.dateRange(price.relation.month)) → MarketValue.quantity(priceAtLevel, price.relation.market.priceUOM)
          }
        }.toMap

        MarketDataEntry(observationDay.atTimeOfDay(market.closeTime), PriceFixingsHistoryDataKey(market),
          PriceFixingsHistoryData.create(data))
      }
    }
  }

  val parentNodes = List(TopRelation.Trafigura.Bloomberg.Futures.Shfe, TopRelation.Trafigura.Bloomberg.Futures.Comex)
  val levels = List(Level.Close, Level.Settle)
}

object LMEFixings extends LimSource {
  type Relation = LMEFixingRelation
  case class LMEFixingRelation(ring: ObservationTimeOfDay, market: CommodityMarket, month:LimMonth)
  case class LimMonth(offset: Int, name: String)

  private val months = List(LimMonth(0, "CASH"), LimMonth(3, "3M"), LimMonth(15, "15M"), LimMonth(27, "27M"))
  private val rings = List(ObservationTimeOfDay.AMR1, ObservationTimeOfDay.Official, ObservationTimeOfDay.PMR1, ObservationTimeOfDay.Unofficial)

  val levels = List(Level.Ask, Level.Bid)

  def relationsFrom(connection: LIMConnection) = {
    for (market <- FuturesExchangeFactory.LME.markets; ring <- rings; month <- months) yield {
      val relation = "TRAF.LME.%s.%s.%s" % (market.commodity.name.toUpperCase, ring.name.toUpperCase, month.name)
      (LMEFixingRelation(ring, market, month), relation)
    }
  }

  def marketDataEntriesFrom(fixings: List[Prices[LMEFixingRelation]]): List[MarketDataEntry] = {
    val groupedFixings = fixings.groupInto(keyGroup, fixing => (fixing.relation.month, fixing.priceByLevel))

    groupedFixings.map { case ((observationPoint, market), fixingsInGroup) =>
      val data = fixingsInGroup.flatMap { case (month, priceByLevel) =>
        priceByLevel.toList.map { case (level, price) =>
          (level, StoredFixingPeriod.tenor(Tenor(Month, month.offset))) → MarketValue.quantity(price, market.priceUOM)
        }
      }.toMap

      MarketDataEntry(
        observationPoint,
        PriceFixingsHistoryDataKey(market),
        PriceFixingsHistoryData.create(data)
      )
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

object LIBORFixings extends HierarchicalLimSource {
  type Relation = LIBORFixingRelation

  case class LIBORFixingRelation(interestRateType: String, currency: UOM, period: StoredFixingPeriod) {
    val group = (interestRateType, currency)
  }

  private val Regex = """TRAF\.(\w+)\.(\w+)\.(\w+)""".r
  private val TenorRegex = """(\d+)(\w)""".r

  val parentNodes = List(TopRelation.Trafigura.Bloomberg.InterestRates.Libor, TopRelation.Trafigura.Bloomberg.InterestRates.Swaps)
  val levels = List(Level.Close)

  def fixingRelationFrom(childRelation: String): Option[(LIBORFixingRelation, String)] = childRelation partialMatch {
    case Regex(rateType, currency, tenor) if parseTenor(tenor).isDefined =>
      (LIBORFixingRelation(rateType, UOM.fromString(currency), StoredFixingPeriod.tenor(parseTenor(tenor).get)), childRelation)
  }

  def marketDataEntriesFrom(fixings: List[Prices[LIBORFixingRelation]]): List[MarketDataEntry] = {
    fixings.groupBy(group(_)).map { case ((rateType, currency, observationDay), grouped) =>
      MarketDataEntry(observationDay.atTimeOfDay(ObservationTimeOfDay.LiborClose),
        PriceFixingsHistoryDataKey(currency.toString, Some(rateType)),
        PriceFixingsHistoryData.create(grouped.map(fixings => (Level.Close, fixings.relation.period) → marketValue(fixings)))
      )
    }.toList
  }

  private def parseTenor(tenor: String): Option[Tenor] = tenor partialMatch {
    case TenorRegex(value, tenorType) => Tenor(TenorType.typesByShortName(tenorType), value.toInt)
  }

  def group(fixings: Prices[LIBORFixingRelation]) = {
    (fixings.relation.interestRateType, fixings.relation.currency, fixings.observationDay)
  }

  def marketValue(fixings: Prices[LIBORFixingRelation]) = MarketValue.percentage(fixings.priceByLevel(Level.Close) / 100)
}

case class LIBORFixing(value: Quantity, fixingDay: Day) {
  val currency = value.uom
  lazy val calendar = LIBORFixing.calendars.getOrElse(currency, throw new Exception("No calendar for: " + this))
  private lazy val gbpCalendar = LIBORFixing.calendars(GBP)

  def valueDay(tenor: Tenor = Tenor(Day, 0)): Day = (tenor, currency) match {
    case (_, GBP)                  => fixingDay
    case (Tenor.ON, ONCurrency(_)) => fixingDay
    case (_, EUR)                  => fixingDay.addBusinessDays(calendar, 2)
    case _                         => fixingDay.addBusinessDays(gbpCalendar, 2).thisOrNextBusinessDay(calendar && gbpCalendar)
  }

  def currency_=(currency: UOM) = copy(value.copy(uom = currency))

  private object ONCurrency {
    def unapply(currency: UOM): Option[UOM] = currency.isOneOf(USD, CAD, EUR, GBP).toOption(currency)
  }
}

object LIBORFixing {
  import HolidayTablesFactory._
  val calendars = {
    Map(
      // O/N
      CAD → BusinessCalendarSet("CAD LIBOR", Location.Unknown, Set(21 Feb 2011, 23 May 2011, 1 Jul 2011, 1 Aug 2011, 5 Sep 2011, 10 Oct 2011, 11 Nov 2011)),
      EUR → BusinessCalendarSet("EUR LIBOR", Location.Unknown, Set(22 Apr 2011, 25 Apr 2011, 29 Aug 2011, 26 Dec 2011, 27 Dec 2011, 02 Jan 2012)),
      GBP → BusinessCalendarSet("GBP LIBOR", Location.London,  Set(3 Jan 2011, 22 Apr 2011, 25 Apr 2011, 29 Apr 2011, 2 May 2011, 30 May 2011, 29 Aug 2011, 26 Dec 2011, 27 Dec 2011, 2 Jan 2012)),
      USD → BusinessCalendarSet("USD LIBOR", Location.Unknown, Set(17 Jan 2011, 21 Feb 2011, 4 Jul 2011, 5 Sep 2011, 10 Oct 2011, 11 Nov 2011, 24 Nov 2011)),

      // S/N
      AUD → BusinessCalendarSet("AUD LIBOR", Location.Unknown, Set(26 Jan 2011, 13 Jun 2011, 1 Aug 2011, 3 Oct 2011)),
      CHF → BusinessCalendarSet("CHF LIBOR", Location.Unknown, Set(2 Jun 2011, 13 Jun 2011, 1 Aug 2011)),
      DKK → BusinessCalendarSet("DKK LIBOR", Location.Unknown, Set(20 May 2011, 2 Jun 2011, 13 Jun 2011)),
      JPY → BusinessCalendarSet("JPY LIBOR", Location.Unknown, Set(10 Jan 2011, 11 Feb 2011, 21 Mar 2011, 3 May 2011, 4 May 2011, 5 May 2011, 18 Jul 2011, 19 Sep 2011, 23 Sep 2011, 10 Oct 2011, 3 Nov 2011, 23 Nov 2011)),
      NZD → BusinessCalendarSet("NZD LIBOR", Location.Unknown, Set(24 Jan 2011, 31 Jan 2011, 6 Jun 2011, 24 Oct 2011)),
      SEK → BusinessCalendarSet("SEK LIBOR", Location.Unknown, Set(6 Jan 2011, 2 Jun 2011, 24 Jun 2011))
    )
  }
}