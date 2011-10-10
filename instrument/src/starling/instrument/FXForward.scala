package starling.instrument

import starling.richdb.RichInstrumentResultSetRow
import starling.quantity.{Quantity, UOM}
import starling.daterange.{Day, DayAndTime}
import starling.instrument.CashInstrumentType._
import starling.curves._
import starling.daterange.DateRangePeriod
import starling.quantity.NamedQuantity
import starling.quantity.UOM.USD

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
case class FXForward(
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
    new CashInstrument(Quantity(1.0, receiveAmount.uom), maturityDate) -> receiveAmount.value,
    new CashInstrument(Quantity(1.0, payAmount.uom), maturityDate) -> payAmount.value
  ))

  def instrumentType = FXForward
  def tradeableType = FXForward

  def explanation(env : Environment) : NamedQuantity = {
    val namedEnv = env.withNaming()
    val (r, p) = (receiveAmount.named("Rec"), payAmount.named("Pay"))

    def timesFx(q : NamedQuantity) = if (q.uom == valuationCCY) q else q * namedEnv.forwardFXRate(valuationCCY, q.uom, maturityDate)
    val discount = namedEnv.discount(valuationCCY, maturityDate).named("Discount")
    (timesFx(r) + timesFx(p)) * discount
  }

    

  /**
   * The amount we are going to receive, always positive.
   */
  private[this] def receiveAmount : Quantity = {
    if(volume.isPositve) {
      volume
    } else {
      volume * -correctStrike
    }
  }

  /**
   * The amount we are going to pay, always negative.
   */
  private[this] def payAmount : Quantity = {
    if(volume.isNegative) {
      volume
    } else {
      volume * -correctStrike
    }
  }

  //FXForward is a UTP even though the asUTPPortfolio method returns a pair of CashInstrument
  //This is because FXForward is used as the forwardState for an FXOption

  def assets(env: Environment) = Assets(
    Asset.knownCash(maturityDate, receiveAmount, env),
    Asset.knownCash(maturityDate, payAmount, env))

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
