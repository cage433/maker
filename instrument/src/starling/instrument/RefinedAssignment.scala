package starling.instrument

import starling.daterange.{Month, Day, DayAndTime}
import starling.richdb.RichInstrumentResultSetRow
import starling.market.{Market, FuturesMarket}
import starling.quantity.{UOM, Quantity}
import starling.curves.Environment
import starling.daterange.DateRangePeriod

case class RefinedAssignment(
  market : FuturesMarket,
  estimatedDeliveryDate : Day,
  volume : Quantity
)
  extends UTP with Tradeable
{
  // TODO [11 Aug 2010] Confirm with Chris G that this is correct
  private val valuationPeriod = market.tenor match {
    case Month => market.frontPeriod(estimatedDeliveryDate)
    case Day => estimatedDeliveryDate
  }


  def isLive(dayAndTime: DayAndTime) = estimatedDeliveryDate.endOfDay > dayAndTime

  def valuationCCY = market.currency

  def assets(env: Environment) = {
    market.convert(volume, market.uom) match {
      case Some(volumeInMarketUnits) => {
        Assets(
          Asset.estimatedPhysical(
            market.toString,
            estimatedDeliveryDate,
            volumeInMarketUnits,
            price(env) * volumeInMarketUnits)
          )
      }
      case None => throw new Exception("Volume UOM = " + volume.uom + ", market uom = " + market.uom)
    }

  }

  def detailsForUTPNOTUSED : Map[String, Any] = persistedTradeableDetails - "Quantity"

  def instrumentType = RefinedAssignment

  def persistedTradeableDetails : Map[String, Any] = Map("EstimatedDelivery" -> estimatedDeliveryDate, "Market" -> market, "Quantity" -> volume)

  def asUtpPortfolio(tradeDay:Day) = UTP_Portfolio(Map(copy(volume = Quantity(1.0, volume.uom)) -> volume.value))

  def tradeableType = RefinedAssignment

  override def expiryDay = Some(estimatedDeliveryDate)

  def dailyVolumes(marketDay: DayAndTime) = Map((estimatedDeliveryDate, market) -> volume)
  def daysForPositionReport(marketDay : DayAndTime) : Seq[Day] = List(estimatedDeliveryDate)

  override def forwardState(env: Environment, dayAndTime: DayAndTime) = throw new Exception("Can't be supported due to lack of fixings")

  def * (scale : Double) = copy(volume = volume * scale)

  def periodKey = Some(DateRangePeriod(estimatedDeliveryDate))

  def price(env : Environment) = {
    env.forwardPrice(market, valuationPeriod)
  }

}


object RefinedAssignment extends InstrumentType[RefinedAssignment] with TradeableType[RefinedAssignment] {
   val name = "Refined Physical"

  def createTradeable(row: RichInstrumentResultSetRow) : RefinedAssignment = {
    val market: FuturesMarket = row.getFuturesMarket("Market")
    val estimatedDeliveryDate = row.getDay("EstimatedDelivery")
    val volume = row.getQuantity("Quantity")

    RefinedAssignment(market, estimatedDeliveryDate, volume)
  }

  def sample = RefinedAssignment(Market.LME_COPPER, Day(2010, 1, 1), Quantity(100, UOM.MT))

}
