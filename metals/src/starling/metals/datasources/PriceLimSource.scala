package starling.metals.datasources

import starling.daterange._
import starling.marketdata._
import starling.utils.ImplicitConversions._
import starling.db.MarketDataEntry
import starling.market._
import starling.lim.{LIMService, LimNode, LIMConnection}
import collection.immutable.List
import scalaz.Scalaz._


abstract class PriceLimSource(val exchange: FuturesExchange, val node: LimNode) extends LimSource(List(Level.Close))

class LMEPriceLimSource extends PriceLimSource(FuturesExchangeFactory.LME, LIMService.TopRelation.Trafigura.Bloomberg.Metals.Lme) {
  type Relation = Nothing
  def description = List(node.name + " TRAF.LME.* (Close)")

  case class LMEPrice(market: FuturesMarket, day: Day) {
    def priceFor(price: Double): (DateRange, Double) = day → (price * market.limSymbol.fold(_.multiplier, 1.0))

    override def toString = "TRAF.LME.%S.%s" % (market.commodity.limName, day.toString("yyyy.MM.dd"))
  }

  override def marketDataEntriesFrom(connection: LIMConnection, start: Day, end: Day) = {
    val futureDays = futureDaysFrom(start)

    def relationsFor(market: FuturesMarket): List[LMEPrice] = futureDays.map(LMEPrice(market, _)).toList

    val pricesByMarket = exchange.markets.toMapWithValues { market =>
      connection.typedPrices(relationsFor(market), Level.Close, start, end)
    }

    pricesByMarket.mapNested { case (market, observationDay, prices) =>
      MarketDataEntry(observationDay.atTimeOfDay(exchange.closeTime), PriceDataKey(market),
        PriceData.create(prices.map { case (relation, price) => relation.priceFor(price) }, market.priceUOM))
    }.toList
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
}

class MonthlyPriceLimSource(node: LimNode, exchange: FuturesExchange) extends PriceLimSource(exchange, node) {
  type Relation = Nothing
  def description = Nil

  case class MonthlyRelation(market: FuturesMarket, limSymbol: LimSymbol, deliveryMonth: Month) {
    def priceFor(price: Double): (DateRange, Double) = deliveryMonth → (price * limSymbol.multiplier)

    override def toString = "TRAF.%S.%S_%s" % (exchange.name, limSymbol.name, deliveryMonth.toInverseReutersString)
  }

  override def marketDataEntriesFrom(connection: LIMConnection, start: Day, end: Day) = {
    val pricesByMarket = exchange.markets.toMapWithSomeValues(relationsFor(_, start)).mapValues { relations =>
      connection.typedPrices(relations, Level.Close, start, end)
    }

    pricesByMarket.mapNested { case (market, observationDay, prices) =>
      MarketDataEntry(observationDay.atTimeOfDay(exchange.closeTime), PriceDataKey(market),
        PriceData.create(prices.map { case (relation, price) => relation.priceFor(price) }, market.priceUOM))
    }.toList
  }

  private def relationsFor(market: FuturesMarket, start: Day) = market.limSymbol.map { limSymbol =>
    deliveryMonthsFrom(start.containingYear).map(deliveryMonth => MonthlyRelation(market, limSymbol, deliveryMonth))
  }

  private def deliveryMonthsFrom(startYear: Year): List[Month] = (startYear upto 7).flatMap(_.toListOfMonths)
}