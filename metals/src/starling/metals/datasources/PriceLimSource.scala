package starling.metals.datasources

import starling.daterange._
import starling.marketdata._
import starling.utils.ImplicitConversions._
import starling.db.MarketDataEntry
import starling.market._
import starling.lim.{LIMService, LimNode, LIMConnection}
import collection.immutable.List
import scalaz.Scalaz._
import starling.pivot.MarketValue


abstract class PriceLimSource(val exchange: FuturesExchange, val node: LimNode) extends LimSource

class LMEPriceLimSource extends PriceLimSource(FuturesExchangeFactory.LME, LIMService.TopRelation.Trafigura.Bloomberg.Metals.Lme) {
  def description = List(node.name + " TRAF.LME.* (Close)")

  def marketDataEntriesFrom(connection: LIMConnection, start: Day, end: Day) = {
    val futureDays = futureDaysFrom(start)

    def relationsFor(market: FuturesMarket): List[Relation] = futureDays.map(Relation(market, _)).toList

    val pricesByMarket = exchange.markets.toMapWithValues { market =>
      connection.getPrices(relationsFor(market), Level.Close, start, end)
    }

    pricesByMarket.mapNested { case (market, observationDay, prices) =>
      MarketDataEntry(observationDay.atTimeOfDay(exchange.closeTime), PriceDataKey(market),
        PriceData.create(prices.map { case (relation, price) => relation.priceFor(price) }, market.priceUOM))
    }
  }

  private def futureDaysFrom(start: Day) = {
    val startMonth = start.containingMonth
    val (days, wednesdays, thirdWednesdays) = (
      (start upto (start.add(3, Month) + 2)).days.filter(_.isWeekday),
      (startMonth upto 6).flatMap(_.daysMatching(DayOfWeek.wednesday)),
      (startMonth upto (10 * 12 + 3)).map(_.thirdWednesday)
    )

    days ++ wednesdays ++ thirdWednesdays
  }.toSet.toList.sortWith(_ < _)

  private case class Relation(market: FuturesMarket, day: Day) {
    def priceFor(price: Double): (DateRange, Double) = day → (price * market.limSymbol.fold(_.multiplier, 1.0))

    override def toString = "TRAF.LME.%S.%s" % (market.commodity.limName, day.toString("yyyy.MM.dd"))
  }
}

class MonthlyPriceLimSource(node: LimNode, exchange: FuturesExchange) extends PriceLimSource(exchange, node)
  with ReutersMonthlyRelations {

  def description = List(node.name + " TRAF.*.*_* (Close)")

  def marketDataEntriesFrom(connection: LIMConnection, start: Day, end: Day) = {
    val pricesByMarket = exchange.markets.toMapWithSomeValues(relationsFor(_, start)).mapValues { relations =>
      connection.getPrices(relations, Level.Close, start, end)
    }

    pricesByMarket.mapNested { case (market, observationDay, prices) =>
      MarketDataEntry(observationDay.atTimeOfDay(exchange.closeTime), PriceDataKey(market),
        PriceData.create(prices.map { case (relation, price) => relation.priceFor(price) }, market.priceUOM))
    }
  }
}

class MonthlyFuturesFixings(node: LimNode, exchange: FuturesExchange) extends LimSource with ReutersMonthlyRelations {
  def description = List("%s TRAF.*.*_* (%s)" % (node.name, exchange.fixingLevel.name))

  def marketDataEntriesFrom(connection: LIMConnection, start: Day, end: Day) = {
    val fixingsByMarket = exchange.markets.toMapWithSomeValues(relationsFor(_, start)).mapValues { relations =>
      connection.getPrices(relations, exchange.fixingLevel, start, end)
    }

    fixingsByMarket.mapNested { case (market, observationDay, fixings) =>
      MarketDataEntry(observationDay.atTimeOfDay(exchange.closeTime), PriceFixingsHistoryDataKey(market),
        PriceFixingsHistoryData.create(fixings.map { case (relation, fixing) => relation.fixingFor(fixing) }))
    }
  }
}

trait ReutersMonthlyRelations {
  protected def relationsFor(market: FuturesMarket, start: Day) = market.limSymbol.map { limSymbol =>
    deliveryMonthsFrom(start.containingYear).map(deliveryMonth => Relation(market, limSymbol, deliveryMonth))
  }

  private def deliveryMonthsFrom(startYear: Year): List[Month] = (startYear upto 7).flatMap(_.toListOfMonths)

  protected case class Relation(market: FuturesMarket, limSymbol: LimSymbol, deliveryMonth: Month) {
    def priceFor(value: Double): (DateRange, Double) = deliveryMonth → (value * limSymbol.multiplier)

    def fixingFor(value: Double): ((Level, StoredFixingPeriod), MarketValue) =
      (market.exchange.fixingLevel, StoredFixingPeriod.dateRange(deliveryMonth)) →
        MarketValue.quantity(value * limSymbol.multiplier, market.priceUOM)

    override def toString = "TRAF.%S.%S_%s" % (market.exchange.name, limSymbol.name, deliveryMonth.toInverseReutersString)
  }
}