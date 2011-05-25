package starling.instrument

import starling.richdb.RichInstrumentResultSetRow
import starling.daterange.{DayAndTime, Day}
import starling.quantity.{UOM, Quantity, Percentage}
import starling.market.{ProxyForwardMarket, ForwardMarket, Market}
import starling.curves._
import starling.models.Call
import starling.models.European
import starling.utils.CollectionUtils
import starling.daterange.DateRangePeriod

/** Represents a purchase of some commodity on some date in the future.
 *  <p>
 *  This code is essentially identical to a futures contract, except that
 *  the mtm is discounted
 */
case class CommodityForward(
  market : ForwardMarket,
  deliveryDay : Day,
  strike : Quantity,
  volume : Quantity
)
  extends UTP with Tradeable with HedgingTradeable
{
  def *(x : Double) = copy(volume = volume * x)

  def assets(env: Environment) = {
    val marketVolume = market.convertUOM(volume, market.uom)
    val marketStrike = market.convertUOM(strike, valuationCCY / market.uom)
    Assets(
      Asset.knownPhysical(market, deliveryDay, marketVolume, env),
      Asset.knownCash(settlementDate, -marketStrike * marketVolume, env) //in practise strike is zero because of the utps
    )
  }

  def valuationCCY : UOM = strike.numeratorUOM

  def isLive(dayAndTime: DayAndTime) : Boolean = dayAndTime < deliveryDay.endOfDay // TODO what should this be - need Steven to look at forwards

  override def expiryDay() = Some(deliveryDay)

  def instrumentType = CommodityForward
  def tradeableType = CommodityForward

  //TODO - check this
  private lazy val settlementDate = deliveryDay
  def details :Map[String, Any] = Map("Market" -> market, "Period" -> deliveryDay, "Strike" -> strike)
  def tradeableDetails :Map[String, Any] = Map("Market" -> market, "Period" -> deliveryDay, "Initial Price" -> strike, "Quantity" -> volume)

  def asUtpPortfolio(tradeDay:Day) = asUtpPortfolio
  def asUtpPortfolio = UTP_Portfolio(
    Map(
      CommodityForward(market, deliveryDay, Quantity(0.0, strike.uom), Quantity(1.0, volume.uom)) -> volume.value,
      new CashInstrument(Quantity(1.0, valuationCCY), settlementDate) -> (- strike * volume).value
    ))

  def daysForPositionReport(marketDay : DayAndTime) : Seq[Day] = List(deliveryDay)

  def periodKey = Some(DateRangePeriod(deliveryDay))


  def price(env : Environment) = {
    env.forwardPrice(market, market.underlying(deliveryDay))
  }
}

object CommodityForward extends InstrumentType[CommodityForward] with TradeableType[CommodityForward] {
  val name = "Commodity Forward"

  def createTradeable(row: RichInstrumentResultSetRow) = {
    CommodityForward(row.getForwardMarket("Market"), row.getDeliveryDay("Period"), row.getQuantity("InitialPrice"), row.getQuantity("Quantity"))
  }
  val leadMarket = Market.LME_LEAD
  def sample = {
    import starling.quantity.Quantity._
    import starling.quantity.UOM._
    val forwardLeadMarket = new ProxyForwardMarket(leadMarket)
    CommodityForward(forwardLeadMarket, Day(2009, 9, 9), 77(USD/MT), 111(MT))
  }
}
