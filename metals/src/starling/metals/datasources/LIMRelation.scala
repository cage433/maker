package starling.metals.datasources

import starling.market._
import starling.utils.ImplicitConversions._
import java.lang.String
import starling.daterange._
import starling.utils.Log
import starling.lim.{LIMService, LimNode}
import starling.utils.Pattern.Extractor
import scalaz._
import Scalaz._
import com.lim.mimapi.RelType

trait LIMRelation extends Log {
  val exchange: FuturesExchange
  val node: LimNode
  val relTypes: Set[RelType] = Set(RelType.CATEGORY)

  def parse(childRelation: String): Option[LimPrice] = try {
    extractor.unapply(childRelation).flatOpt
  } catch {
    case _ => debug[LimPrice](getClass.getSimpleName + " could not parse: " + childRelation)
  }

  protected val extractor: Extractor[String, Option[LimPrice]]
  protected def debug[T](message: String): Option[T] = { log.debug(message); None }
}

case class LimPrice(market: CommodityMarket, period: DateRange, observationTimeOfDay: ObservationTimeOfDay)

object CMPTLIMRelation extends LIMRelation {
  val node = LIMService.TopRelation.Trafigura.Bloomberg.Currencies.Composite
  val exchange = FuturesExchangeFactory.SHFE

  // TODO [10 Jun 2011] Change PriceDataKey so that it uses a market name
  private val limPriceTemplate = LimPrice(null, null, exchange.closeTime)

  protected val extractor = Extractor.regex[Option[LimPrice]]("""TRAF\.CFETS\.CNY\.(\w+)""") { case List(reutersDeliveryMonth) =>
    ReutersDeliveryMonthCodes.parse(reutersDeliveryMonth).map(month => limPriceTemplate.copy(period = month))
  }
}