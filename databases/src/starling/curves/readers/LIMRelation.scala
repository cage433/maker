package starling.curves.readers

import starling.market._
import starling.utils.ImplicitConversions._
import java.lang.String
import starling.daterange._
import starling.utils.Log
import starling.{LIMServer, LimNode}
import starling.utils.Pattern.Extractor
import scalaz._
import Scalaz._

trait LIMRelation extends Log {
  val node: LimNode

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
  val node = LIMServer.TopRelation.Trafigura.Bloomberg.Currencies.Composite

  // TODO [10 Jun 2011] Change PriceDataKey so that it uses a market name
  private val limPriceTemplate = LimPrice(null, null, ObservationTimeOfDay.SHFEClose)

  protected val extractor = Extractor.regex[Option[LimPrice]]("""TRAF\.CFETS\.CNY\.(\w+)""") { case List(reutersDeliveryMonth) =>
    ReutersDeliveryMonthCodes.parse(reutersDeliveryMonth).map(month => limPriceTemplate.copy(period = month))
  }
}