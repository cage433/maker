package starling.instrument

import physical.{UnallocatedSalesQuota, PhysicalMetalAssignment}
import starling.daterange.{DateRange, DayAndTime, Day}
import starling.richdb.RichInstrumentResultSetRow
import starling.utils.ImplicitConversions._
import starling.curves.Environment
import starling.quantity.{NamedQuantity, SpreadOrQuantity, Quantity}
import starling.quantity.UOM
import starling.market.{FuturesExchange, IndexWithDailyPrices}
import starling.market.rules.{RoundingMethodRule, SwapPricingRule}

trait Tradeable extends AsUtpPortfolio {
  def tradeableType : TradeableType[_]
  def persistedTradeableDetails : Map[String, Any]
  def shownTradeableDetails: Map[String, Any] = persistedTradeableDetails
  def expiryDay():Option[Day] = None
  def isLive(dayAndTime : DayAndTime) : Boolean
  def valuationCCY : UOM

  def explanation(env: Environment): NamedQuantity

  /**
   * Hack so that for Jons option the premium has an associated mEnvironmentarket/index + period
   * so P&L and Theta have a Risk Market and Risk Period
   */
  def fixUpCashInstruments(ci: CashInstrument): CashInstrument = {
     // sometimes the cash instrument has already been assigned to a market and period and we don't want to override that here
    if(ci.index.isEmpty && ci.averagingPeriod.isEmpty)
      fixUpMyCashInstruments(ci)
    else
      ci
  }

  protected def fixUpMyCashInstruments(ci: CashInstrument): CashInstrument = ci
}

trait TradeableType[T <: Tradeable] {
  val name:String
  override def toString = name
  def createTradeable(row: RichInstrumentResultSetRow): T
  def sample:T
  def fields:List[String] = {
    val tradeableFields = sample.shownTradeableDetails.keySet.map(_.removeWhiteSpace.toLowerCase).toList
    val allConvertedFields = TradeableType.fields.map(_.removeWhiteSpace.toLowerCase)
    val matchingFields = allConvertedFields.intersect(tradeableFields)
    matchingFields.map(field => TradeableType.fields(allConvertedFields.indexOf(field)))
  }
}

trait HedgingTradeable extends Tradeable {
  def asUtpPortfolio():UTP_Portfolio
}

object TradeableType {
  val types = List[TradeableType[_ <: Tradeable]](
    DeletedInstrument,
    ErrorInstrument,
    Future,
    TAS,
    FuturesCalendarSpread,
    FuturesCommoditySpread,
    CommoditySwap,
    SwapCalendarSpread,
    FuturesOption,
    CalendarSpreadOption,
    CommoditySpreadOption,
    AsianOption,
    FXForward,
    RefinedAssignment,
    RefinedFixationsForSplit,
    CashInstrument,
    PhysicalMetalAssignment,
    UnallocatedSalesQuota
  )
  def fromName(name : String) = types.find(_.name == name) match {
    case Some(t) => types.find(_.name == name).get // some scala bug means have to do it this way
    case None => throw new Exception("Couldn't find trade with name " + name)
  }
  //the union of the keys in the Instrument#details method
  val fieldsWithType = List(  //This is the order in which the fields will be shown in the GUI
    ("Market", classOf[String]),
    ("Period", classOf[DateRange]),
    ("Quantity", classOf[Quantity]),
    ("Initial Price",classOf[SpreadOrQuantity]),
    ("Strike",classOf[Quantity]),
    ("Exercise Day",classOf[Day]),
    ("Maturity Day", classOf[Day]),
    ("Delivery Day", classOf[Day]),
    ("Call Put", classOf[String]),
    ("Exercise Type", classOf[String]),
    ("Cleared", classOf[Boolean]),
    ("PricingRule", classOf[SwapPricingRule]),
    ("RoundingMethodRule", classOf[RoundingMethodRule]),
    ("Error", classOf[String]),
    ("Estimated Delivery", classOf[Day]),
    ("Fixations", classOf[List[RefinedFixation]]),
    ("Cash Instrument Type", classOf[CashInstrumentType]),
    ("Commodity", classOf[String]),
    ("Contract Index", classOf[Option[IndexWithDailyPrices]]),
    ("Premium", classOf[Quantity]),
    ("Exchange", classOf[FuturesExchange]),
    ("Inventory ID", classOf[String]),
    ("Inventory Quantity", classOf[Quantity]),
    ("Assignment ID", classOf[String]) ,
    ("Direction", classOf[String]),
    ("Contract Pricing Spec Name", classOf[String]),
    ("Contract Delivery Day", classOf[Day]),
    ("Contract Location Code", classOf[String]),
    ("Contract Inco Term Code", classOf[String]),
    ("Benchmark Delivery Day", classOf[Day]),
    ("Benchmark Country Code", classOf[String]),
    ("Benchmark Inco Term Code", classOf[String]),
    ("Grade Code", classOf[String])


  )
  val fields = fieldsWithType.map(_._1)
  val drillDownFields = fields.filterNot(List("Float Payment Freq", "Fixed Basis", "Fixed Payment Freq", "Fixed Rate",
    "Float Basis", "First Spread Period", "Second Spread Period").contains(_))
  val lowercaseNoSpaceFields = fieldsWithType.map(_._1.toLowerCase.replaceAll(" ", ""))
}
