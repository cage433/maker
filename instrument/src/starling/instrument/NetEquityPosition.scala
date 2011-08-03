package starling.instrument

import starling.market.{RIC, EquityPriceKey}
import starling.quantity.{UOM, Quantity, Percentage}
import starling.richdb.RichInstrumentResultSetRow
import starling.daterange.{Day, DayAndTime}
import starling.curves.Environment
import starling.market.EquityPriceCurveKey
import starling.quantity.NamedQuantity

/**
 * Represents a net position in shares
 *
 * Used for the special situations fund VAR
 */

object NetEquityPosition extends InstrumentType[NetEquityPosition] with TradeableType[NetEquityPosition] {
  def sample = NetEquityPosition(RIC("FOO"), Quantity(100, UOM.SHARE))

  def createTradeable(row: RichInstrumentResultSetRow) = {
    new NetEquityPosition(RIC(row.getString("RIC")), row.getQuantity("Quantity"))
  }
  val name = "Net Equity Position"
}

case class NetEquityPosition(ric:RIC, volume:Quantity) extends UTP with Tradeable with HedgingTradeable {
  def valuationCCY = ric.currency
  def price(env: Environment): Quantity = {
    env.equityPrice(ric)
  }
  def explanation(env : Environment) : NamedQuantity = throw new UnsupportedOperationException()

  def *(x : Double) = copy(volume = volume * x)

  def assets(env: Environment) = {
    val F = price(env)
    Assets(Asset.estimatedCash(env.marketDay.day, F * volume, F * volume))
  }
  override def expiryDay() = Some(Day(2050, 1, 1)) //HACK
  def isLive(dayAndTime: DayAndTime) = true
  def persistedTradeableDetails = Map("ric"->ric.code, "Quantity"->volume)
  def asUtpPortfolio(tradeDay:Day):UTP_Portfolio = asUtpPortfolio
  def asUtpPortfolio():UTP_Portfolio = UTP_Portfolio(Map(this.copy(volume=Quantity(1, UOM.SHARE)) -> volume.value))
  def tradeableType = NetEquityPosition
  def detailsForUTPNOTUSED = persistedTradeableDetails
  def instrumentType = NetEquityPosition

  def daysForPositionReport(marketDay : DayAndTime) : Seq[Day] = List(marketDay.day)
  def periodKey = None
}
