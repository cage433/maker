package starling.instrument

import starling.richdb.RichInstrumentResultSetRow
import starling.daterange._
import starling.curves._
import starling.quantity.Quantity
import starling.quantity.Quantity._
import starling.models._
import starling.daterange.DateRangePeriod
import starling.market.{Market, FuturesMarket}
import starling.quantity.NamedQuantity
import starling.quantity.Percentage
import starling.quantity.PercentageNamedQuantity
import starling.quantity.SimpleNamedQuantity
import starling.quantity.FunctionNamedQuantity


case class FuturesOption(
  market : FuturesMarket, 
  exerciseDay : Day,
  delivery : DateRange,
  strike : Quantity, 
  volume : Quantity, 
  callPut : CallOrPut,
  exerciseType: ExerciseType
) extends UTP with Tradeable
{
  require (strike.value > 0, "Non positive strike")
  require(strike.denominatorUOM == market.uom, "Can't handle strike in non-market uom, strike: " + strike + ", market: " + market.uom)
  require(volume.uom == market.uom, "Can't handle volume in non-market uom, volume: " + volume + ", market: " + market.uom)

	val valuationCCY = strike.numeratorUOM.toBaseCurrency


  override def forwardState(env: Environment, dayAndTime: DayAndTime) = {
    // if in the normal environment we are still live, and in the forward state we are after exercise:
    if (env.marketDay < exerciseDay.endOfDay && dayAndTime >= exerciseDay.endOfDay) {
      val callPutSign = callPut match { case Call => 1.0; case Put => -1.0}
      callPut.payoff(env.forwardState(exerciseDay.endOfDay).forwardPrice(market, delivery), strike) match {
        case Some(_) => Future(market, delivery, strike, volume * callPutSign)
        case None => this
      }
    } else {
      this
    }
  }

  def assets(env : Environment) = {
    if(isLive(env.marketDay)) {
      val mtm = price(env) * volume
      Assets(Asset.estimatedCash(env.marketDay.day, mtm, mtm))
    } else {
      Assets()
    }
	}

  override def expiryDay() = Some(exerciseDay)

	def isLive(dayAndTime : DayAndTime) : Boolean = dayAndTime < exerciseDay.endOfDay
  
  def detailsForUTPNOTUSED :Map[String, Any] = Map("Market" -> market, "ExerciseDay" -> exerciseDay, "Period" -> delivery, "Strike" -> strike, "CallPut" -> callPut, "ExerciseType" -> exerciseType)
  def persistedTradeableDetails:Map[String, Any] = Map("Market" -> market, "ExerciseDay" -> exerciseDay, "Period" -> delivery, "Strike" -> strike, "Quantity" -> volume, "CallPut" -> callPut, "ExerciseType" -> exerciseType)

  def asUtpPortfolio(tradeDay:Day) = UTP_Portfolio(Map(new FuturesOption(market, exerciseDay, delivery, strike, Quantity(1.0, volume.uom), callPut, exerciseType) -> volume.value))

  def instrumentType = FuturesOption
  def tradeableType = FuturesOption

  private def americanOptionPrice(env : Environment) : Quantity = {
    val zeroRate = env.zeroRate(valuationCCY, exerciseDay).decimalValue
    val F = env.forwardPrice(market, delivery)
    val vol = env.impliedVol(market, delivery, exerciseDay, strike).nonZero
    val K = strike.value
    val solver = new CrankNicholsonOptionPricer(env.environmentParameters, env.marketDay, exerciseDay, F.value, vol.decimalValue, zeroRate, callPut, K.value)
    val price = solver.valueAmericanWithCorrection
    Quantity(price, F.uom)
  }

  def daysForPositionReport(marketDay : DayAndTime) : Seq[Day] = List(exerciseDay)

  def * (scale : Double) = copy(volume = volume * scale)

  def periodKey = Some(DateRangePeriod(delivery))

  override def riskMarketExtra = String.format("%6.2f%n ", new java.lang.Double(strike.value)) + callPut.toShortString
  override def atomicKeyCachingUTP : UTP = copy(strike = 1.0(market.priceUOM))

  override def interpolatedVol(env : Environment, volKey : EnvironmentDifferentiable with VolKey) : Quantity = {
    volKey match {
      case _ : BradyMetalVolAtomicDatumKey =>
      case _ : OilAtmVolAtomicDatumKey =>
      case _ => throw new Exception("Unexpected vol key " + volKey)
    }
    new Quantity(env.impliedVol(market, delivery, exerciseDay, strike).decimalValue)
  }


  def explanation(env : Environment) : NamedQuantity = {
    val (_, (undiscountedOptionPrice, underlyingPrice, vol, time, discount)) = priceWithDetails(env.withNaming())
    FunctionNamedQuantity("FuturesOption-" + exerciseType + "-" + callPut, List(underlyingPrice, strike.named("K"), vol, time), undiscountedOptionPrice, true) * volume.named("Volume") * discount
  }

  def price(env: Environment) = priceWithDetails(env)._1
  
  private def priceWithDetails(env : Environment) : (Quantity, (Quantity, NamedQuantity, NamedQuantity, NamedQuantity, NamedQuantity)) = {
    val (discountedOptionPrice, underlyingPrice, vol, time, discount) = if (isLive(env.marketDay)) {
      val F = env.forwardPrice(market, delivery)
      val vol = env.impliedVol(market, delivery, exerciseDay, strike)
      val T = exerciseDay.endOfDay.timeSince(env.marketDay)
      val discount = env.discount(valuationCCY, exerciseDay)
      val discountedOptionPrice = if (exerciseDay.endOfDay == env.marketDay)
        callPut.intrinsicPrice(F, strike)
      else exerciseType match {
        case American => americanOptionPrice(env)
        case European => {
          BlackScholes.undiscountedOptionPrice(F, strike, callPut, T, vol) * discount
        }
      }
      (discountedOptionPrice, F, vol, T, discount)
    } else {
      (Quantity(0, market.priceUOM), Quantity.NULL, Percentage(0), 0.0, new Quantity(1.0))
    }
    (
      discountedOptionPrice,
      (discountedOptionPrice / discount, underlyingPrice.named("F"), PercentageNamedQuantity("Vol", vol), SimpleNamedQuantity("T", new Quantity(time)), discount.named("Discount"))
    )
  }

  override def fixUpMyCashInstruments(ci: CashInstrument) = {
    ci.copy(index = Some(Right(market)), averagingPeriod = Some(DateRangePeriod(delivery)))
  }
}

object FuturesOption extends InstrumentType[FuturesOption] with TradeableType[FuturesOption] {
  val id = 4
  val name = "Futures Option"

  def createTradeable(row: RichInstrumentResultSetRow) = {
    val futuresMarket = row.getFuturesMarket("Market")
    new FuturesOption(futuresMarket, row.getDay("ExerciseDay"), row.getDateRange("Period", Some(futuresMarket.tenor)), row.getQuantity("Strike"), row.getQuantity("Quantity"), row.getCallPut("CallPut"), row.getExerciseType("exercisetype"))
  }
  def sample = {
    val leadMarket = Market.LME_LEAD
    import starling.quantity.UOM._
    new FuturesOption(leadMarket, Day(2009, 8, 1), Day(2009, 8, 20), 98(USD/MT), 333(MT), Call, European)
  }
}
