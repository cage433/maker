package starling.instrument

import starling.richdb.RichInstrumentResultSetRow
import starling.quantity.{Quantity, UOM}
import starling.daterange.{Day, DayAndTime}
import starling.instrument.CashInstrumentType._
import starling.curves._
import starling.daterange.DateRangePeriod

/**
 * This trait is extended by both FXForward and FXOption. It deals with
 *    a) the problem that in Trinity the strike may be inverted, the correct strike can only be determined from the uom of the volume currency.
 *    b) checking the consistency of strike and volume uoms
 *    c) testing whether the instrument is live, since the rule is the same for both
 */
trait FXTradeable extends Tradeable {
  def strike : Quantity
  def volume : Quantity
  def maturityDate : Day

  protected[this] def correctStrike = {
    if(volume.uom == strike.numeratorUOM)
      strike.invert
    else
      strike
  }

  def valuationCCY : UOM = (correctStrike * volume).uom

  assert(volume.uom == correctStrike.denominatorUOM, "Volume currency ("+volume.uom+") not the same as strike: " + strike.uom)

  def daysForPositionReport(marketDay : DayAndTime) : Seq[Day] = List(maturityDate)

  def periodKey = Some(DateRangePeriod(maturityDate))

}

/** Represents an FX forward to buy 'volume' of CCY1 at a strike in units of CCY2 / CCY1, on the given maturity date.
 */
case class
FXForward(
  strike : Quantity,
  volume : Quantity, 
  maturityDate : Day
) 
	extends FXTradeable with UTP with HedgingTradeable
{

  def isLive(dayAndTime : DayAndTime) : Boolean = dayAndTime <= maturityDate.startOfDay

  override def expiryDay() = Some(maturityDate)

  def persistedTradeableDetails :Map[String, Any] = Map("MaturityDay" -> maturityDate, "Initial Price" -> strike, "Quantity" -> volume)

  def asUtpPortfolio(tradeDay:Day):UTP_Portfolio = asUtpPortfolio
  def asUtpPortfolio():UTP_Portfolio = UTP_Portfolio(Map(
    new CashInstrument(Quantity(1.0, buyAmount.uom), maturityDate) -> buyAmount.value,
    new CashInstrument(Quantity(1.0, sellAmount.uom), maturityDate) -> sellAmount.value
  ))

  def instrumentType = FXForward
  def tradeableType = FXForward

  /**
   * The amount we are going to buy, always positive.
   */
  private[this] def buyAmount : Quantity = {
    if(volume.isPositve) {
      volume
    } else {
      volume * -correctStrike
    }
  }

  /**
   * The amount we are going to sell, always negative.
   */
  private[this] def sellAmount : Quantity = {
    if(volume.isNegative) {
      volume
    } else {
      volume * -correctStrike
    }
  }

  //FXForward is a UTP even though the asUTPPortfolio method returns a pair of CashInstrument
  //This is because FXForward is used as the forwardState for an FXOption

  def assets(env: Environment) = Assets(
    Asset.knownCash(maturityDate, buyAmount, env),
    Asset.knownCash(maturityDate, sellAmount, env))

  def detailsForUTPNOTUSED = throw new IllegalStateException("This should only be used as a UTP in the context of an FXOption forwardState so it should not be persisted")

  def pivotUTPType = null

  def * (scale : Double) = copy(volume = volume * scale)

  def price(env : Environment) = {
    env.forwardFXRate(correctStrike.uom, maturityDate)
  }
}

object FXForward extends TradeableType[FXForward] with InstrumentType[FXForward] {
  val id = 7
  val name = "FX Forward"

  def createTradeable(row: RichInstrumentResultSetRow) = {
    FXForward(row.getQuantity("InitialPrice"), row.getQuantity("Quantity"), row.getDay("MaturityDay"))
  }

  def sample = {
    import starling.quantity.Quantity._
    import starling.quantity.UOM._
    FXForward(1.5(EUR/USD), 999(USD), Day(2009, 9, 8))
  }
}
