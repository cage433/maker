package starling.curves.readers

import starling.market._
import starling.utils.ImplicitConversions._
import java.lang.String
import starling.daterange._
import starling.utils.Log
import starling.{LIMServer, LimNode}
import starling.utils.Pattern.Extractor

trait LIMRelation extends Log {
  val node: LimNode

  def parse(childRelation: String) = try {
    extractor.unapply(childRelation).flatOpt
  } catch {
    case _ => debug[LimPrice](getClass.getSimpleName + " could not parse: " + childRelation)
  }

  protected val extractor: Extractor[String, Option[LimPrice]]
  protected def debug[T](message: String): Option[T] = { log.debug(message); None }
}

case class LimPrice(market: CommodityMarket, period: DateRange, observationTimeOfDay: ObservationTimeOfDay)

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

object CMPTLIMRelation extends LIMRelation {
  val node = LIMServer.TopRelation.Trafigura.Bloomberg.Currencies.Composite

  // TODO [10 Jun 2011] Change PriceDataKey so that it uses a market name
  private val limPriceTemplate = LimPrice(null, null, ObservationTimeOfDay.SHFEClose)

  protected val extractor = Extractor.regex[Option[LimPrice]]("""TRAF\.CFETS\.CNY\.(\w+)""") { case List(reutersDeliveryMonth) =>
    ReutersDeliveryMonthCodes.parse(reutersDeliveryMonth).map(month => limPriceTemplate.copy(period = month))
  }
}