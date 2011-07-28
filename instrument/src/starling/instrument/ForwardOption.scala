package starling.instrument

import starling.richdb.RichInstrumentResultSetRow
import starling.models._
import starling.daterange.TimeOfDay._
import starling.quantity.Percentage._
import starling.curves._
import starling.daterange.{DateRangePeriod, DayAndTime, Day}
import starling.daterange.DateRangePeriod
import starling.market.{CommodityMarket, Market}
import starling.quantity.{UOM, Quantity}

case class ForwardOption(
  market : CommodityMarket,
  exerciseDay : Day,
  deliveryDay : Day,
  strike : Quantity,
  volume : Quantity,
  callPut : CallOrPut,
  exerciseType: ExerciseType
) extends UTP with Tradeable
{
  require(strike.denominatorUOM == market.uom, "Can't handle strike in non-market uom, strike: " + strike + ", market: " + market.uom)
  require(volume.uom == market.uom, "Can't handle volume in non-market uom, volume: " + volume + ", market: " + market.uom)

  val valuationCCY = strike.uom * market.uom

  lazy val settlementDay = deliveryDay

  override def forwardState(env: Environment, dayAndTime: DayAndTime) = {
    if (dayAndTime < exerciseDay.endOfDay) {
      this
    } else {
      val callPutSign = callPut match { case Call => 1.0; case Put => -1.0}
      callPut.payoff(env.forwardState(exerciseDay.endOfDay).forwardPrice(market, deliveryDay), strike) match {
        case Some(_) => CommodityForward(market, deliveryDay, strike, volume * callPutSign)
        case None => this
      }
    }
  }

  def assets(env : Environment) = {
    if(isLive(env.marketDay)) {
      val disc = env.discount(valuationCCY, exerciseDay).checkedValue(UOM.SCALAR)
      var F = env.forwardPrice(market, deliveryDay)
      val Array(probOfExercise, exercisePriceRatio, undiscProbOfExercise) = pricer(env) match {
        case Left(cn) => cn.americanAssetsWithCorrection
        case Right(bs) => 
          Array(bs.probabilityOfExercise * disc, bs.expectedPriceFractionGivenExercise * disc, bs.probabilityOfExercise)
      }
//      val vol = env.impliedVol(market, deliveryDay, exerciseDay, strike)
//      val T = exerciseDay.timeSince(env.marketDay.day)
//      val r = env.zeroRate(valuationCCY, exerciseDay)
//      var F = price(env)
//      val disc = env.discount(valuationCCY, exerciseDay)
//      val Array(probOfExercise, exercisePriceRatio, undiscProbOfExercise) = exerciseType match {
//        case American =>
//          new CrankNicholsonOptionPricer(
//            env.finiteDifferenceParameters,
//            env.marketDay.day,
//            exerciseDay,
//            F.value,
//            vol,
//            r,
//            callPut,
//            strike.value).americanAssetsWithCorrection
//
//        case European =>
//          {
//            val bs = new BlackScholes(F.value, strike.value, callPut, T, vol)
//            Array(bs.probabilityOfExercise * disc, bs.expectedPriceFractionGivenExercise * disc, bs.probabilityOfExercise)
//          }
//      }
      var assets = Assets(
        Asset.estimatedCash(env.marketDay.day, - strike * undiscProbOfExercise * volume, - strike * probOfExercise * volume),
        Asset.estimatedPhysical(market.toString, exerciseDay, volume * undiscProbOfExercise, F * exercisePriceRatio * volume)
        )
      callPut match {
        case Call => assets
        case Put => assets * -1
      }
    } else {
      Assets()
    }
	}
	private def pricer(env : Environment) : Either[CrankNicholsonOptionPricer, BlackScholes] = {
    val vol = env.impliedVol(market, deliveryDay, exerciseDay, strike)
    val T = exerciseDay.endOfDay.timeSince(env.marketDay)
    val r = env.zeroRate(valuationCCY, exerciseDay)
    var F = env.forwardPrice(market, deliveryDay)
    exerciseType match {
      case American =>
        Left(new CrankNicholsonOptionPricer(
          env.environmentParameters,
          env.marketDay,
          exerciseDay,
          F.value,
          vol,
          r,
          callPut,
          strike.value))

      case European =>
          Right(new BlackScholes(F.value, strike.value, callPut, T, vol))
    }
  }



	def isLive(dayAndTime : DayAndTime) : Boolean = dayAndTime <= exerciseDay.startOfDay

  override def expiryDay() = Some(exerciseDay)

  def detailsForUTPNOTUSED :Map[String, Any] = Map("Market" -> market, "ExerciseDay" -> exerciseDay, "Period" -> deliveryDay, "Strike" -> strike, "CallPut" -> callPut, "ExerciseType" -> exerciseType)
  def persistedTradeableDetails :Map[String, Any] = Map("Market" -> market, "ExerciseDay" -> exerciseDay, "Period" -> deliveryDay, "Strike" -> strike, "Quantity" -> volume, "CallPut" -> callPut, "ExerciseType" -> exerciseType)

  def asUtpPortfolio(tradeDay:Day) = UTP_Portfolio(Map(new ForwardOption(market, exerciseDay, deliveryDay, strike, Quantity(1.0, volume.uom), callPut, exerciseType) -> volume.value))

  def instrumentType = ForwardOption
  def tradeableType = ForwardOption

  def daysForPositionReport(marketDay : DayAndTime) : Seq[Day] = List(exerciseDay)

  def * (scale : Double) = copy(volume = volume * scale)

  def periodKey = Some(DateRangePeriod(deliveryDay))

  def price(env : Environment) = {
    val P = pricer(env) match {
      case Left(cn) => cn.valueAmericanWithCorrection
      case Right(bs) => bs.undiscountedOptionPrice
    }
    Quantity(P, strike.uom)
  }
}

object ForwardOption extends InstrumentType[ForwardOption] with TradeableType[ForwardOption] {
  val name = "Forward Option"

  def createTradeable(row: RichInstrumentResultSetRow) = {
    new ForwardOption(row.getCommodityMarket("Market"), row.getDay("ExerciseDay"), row.getDeliveryDay("Period"), row.getQuantity("Strike"), row.getQuantity("Quantity"), row.getCallPut("CallPut"), row.getExerciseType("exercisetype"))
  }
  def sample = {
    import starling.quantity.UOM._
    new ForwardOption(Market.LME_ALUMINIUM, Day(2009, 8, 1), Day(2009, 8, 20),
      Quantity(100, USD/MT), Quantity(100, MT), Call, European)
  }

}
