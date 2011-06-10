package starling.curves.readers

import starling.market._
import starling.utils.ImplicitConversions._
import java.lang.String
import starling.daterange._
import starling.utils.Log
import starling.LimNode


trait LIMRelation {
  val node: LimNode
  val observationTimeOfDay: ObservationTimeOfDay
  def parse(childRelation: String): Option[LimPrice]

  protected def debug[T](message: String): Option[T] = { Log.debug(message); None }
}

case class LimPrice(futuresMarket: FuturesMarket, period: DateRange, observationTimeOfDay: ObservationTimeOfDay)

class LMELIMRelation(val node: LimNode, val observationTimeOfDay: ObservationTimeOfDay) extends LIMRelation {
  private lazy val lmeMarkets = FuturesExchangeFactory.LME.marketsByCommodityName + "steelbillet" → Market.LME_STEEL_BILLETS
  private val Regex = """TRAF\.LME\.(\w+)\.(\d+)\.(\d+)\.(\d+)""".r

  def parse(childRelation: String) = childRelation.safePartialMatch("Missing LME market for LIM Relation") {
    case Regex(commodity, year, month, day) =>
      LimPrice(lmeMarkets(commodity.toLowerCase), Day(year, month, day), observationTimeOfDay)
  }
}

class MonthlyLIMRelation(val node: LimNode, val observationTimeOfDay: ObservationTimeOfDay) extends LIMRelation {
  private val Regex = """TRAF\.(\w+)\.(\w+)_(\w+)""".r

  def parse(childRelation: String) = (childRelation partialMatch {
    case Regex(exchange, limSymbol, reutersDeliveryMonth) =>
      val optMarket = Market.futuresMarkets.find(market => market.limSymbol.map(_.name) == Some(limSymbol))
      val optMonth = ReutersDeliveryMonthCodes.parse(reutersDeliveryMonth)

      (optMarket, optMonth) match {
        case (Some(market), Some(month)) => Some(LimPrice(market, month, observationTimeOfDay))
        case (None, _) => debug("No Market with exchange: %s and LIM Symbol: %s" % (exchange, limSymbol))
        case (_, None) => debug("Cannot parse Reuters delivery month: " + reutersDeliveryMonth)
      }
  }).flatOpt
}