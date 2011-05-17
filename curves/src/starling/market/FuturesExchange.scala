package starling.market

import java.lang.String
import starling.daterange.ObservationTimeOfDay
import starling.daterange.ObservationTimeOfDay._
import starling.utils.{Named, StarlingEnum}
import starling.utils.ImplicitConversions._

trait DeliveryType {
  val name: String
  override def toString = name
}
case object DailyDelivery extends DeliveryType {
  val name = "Daily Delivery"
}
case object MonthlyDelivery extends DeliveryType {
  val name = "Monthly Delivery"
}

case class FuturesExchange(name: String, deliveryType: DeliveryType, closeTime:ObservationTimeOfDay) extends Named {
  lazy val markets = Market.futuresMarkets.filter(_.exchange == this)
  lazy val marketsByCommodityName = markets.toMapWithKeys(_.commodity.name.toLowerCase)
}

/**
 * Neptune uses contracts on COMEX and LME to price its trades
 */
trait NeptunePricingExchange extends FuturesExchange{
  def inferMarketFromCommodityName(neptuneCommodityName : String) : FuturesMarket
  def inferMarketFromCommodityCode(neptuneCommodityCode : String) : FuturesMarket
}

object NeptunePricingExchange{
  def fromNeptuneCode(code : String) : NeptunePricingExchange = Map(
    "CMX" → FuturesExchangeFactory.COMEX,
    "LME" → FuturesExchangeFactory.LME
  ).get(code) match {
    case Some(exchange) => exchange
    case None => throw new IllegalStateException("No exchange for Neptune code " + code)
  }
}

object FuturesExchangeFactory extends StarlingEnum(classOf[FuturesExchange]) {
  val LME = new FuturesExchange("LME", DailyDelivery, LMEClose) with NeptunePricingExchange{
    def inferMarketFromCommodityName(neptuneCommodityName: String) = neptuneCommodityName match {
      case "Copper"	            => Market.LME_COPPER
      case "Lead"	              => Market.LME_LEAD
      case "Zinc"	              => Market.LME_ZINC
      case "Tin"	              => Market.LME_TIN
      case "Primary Aluminium"	=> Market.LME_ALUMINIUM
      case "Nickel"	            => Market.LME_NICKEL
      case "Aluminium Alloy"	  => Market.LME_ALUMINIUM_ALLOY
      case "Steel"	            => Market.LME_STEEL_BILLETS
      case _ => throw new IllegalStateException("No known LME market for Neptune commodity " + neptuneCommodityName)
    }
    def inferMarketFromCommodityCode(neptuneCommodityCode: String) = neptuneCommodityCode match {
      case "CAD"	            => Market.LME_COPPER
      case "PB"	              => Market.LME_LEAD
      case "ZN"	              => Market.LME_ZINC
      case "SN"	              => Market.LME_TIN
      case "AHD"	            => Market.LME_ALUMINIUM
      case "NI"	              => Market.LME_NICKEL
      case "AA"	              => Market.LME_ALUMINIUM_ALLOY
      case "STL"	            => Market.LME_STEEL_BILLETS
      case _ => throw new IllegalStateException("No known LME market for Neptune commodity code " + neptuneCommodityCode)
    }
  }
  val COMEX = new FuturesExchange("COMEX", MonthlyDelivery, COMEXClose) with NeptunePricingExchange{
    def inferMarketFromCommodityName(neptuneCommodityName: String) = neptuneCommodityName match {
      case "Copper"	            => Market.COMEX_HIGH_GRADE_COPPER
      case _ => throw new IllegalStateException("No known COMEX market for Neptune commodity " + neptuneCommodityName)
    }
    def inferMarketFromCommodityCode(neptuneCommodityCode: String) = neptuneCommodityCode match {
      case "CAD"	            => Market.COMEX_HIGH_GRADE_COPPER
      case _ => throw new IllegalStateException("No known COMEX market for Neptune commodity code " + neptuneCommodityCode)
    }
  }
  val NYMEX = new FuturesExchange("NYMEX", MonthlyDelivery, Default)
  val SFS = new FuturesExchange("SFS", MonthlyDelivery, SHFEClose)
  val BALTIC = new FuturesExchange("Baltic Exchange", MonthlyDelivery, Default)
  val ICE = new FuturesExchange("ICE", MonthlyDelivery, Default)
  val MDEX = new FuturesExchange("MDEX", MonthlyDelivery, Default)
  val EXBXG = new FuturesExchange("EXBXG", MonthlyDelivery, Default)
}