package starling.instrument

import starling.daterange.TimeOfDay._
import starling.richdb.RichInstrumentResultSetRow
import cern.jet.random.Normal
import starling.utils.ImplicitConversions._
import starling.models._
import starling.quantity.Quantity._
import starling.curves.Environment
import starling.daterange._
import starling.market.{ForwardPriceRiskFactorType, Market, FuturesMarket}
import java.lang.String
import starling.utils.Log
import starling.curves.FuturesSpreadPrice
import starling.daterange.SpreadPeriod
import starling.curves.EnvironmentDifferentiable
import starling.curves.VolKey
import starling.curves.SpreadAtmStdDevAtomicDatumKey
import starling.quantity.{NamedQuantity, UOM, Quantity}
import starling.quantity.SimpleNamedQuantity
import starling.quantity.FunctionNamedQuantity

case class CalendarSpreadOption(
        market: FuturesMarket,
        period: Period,
        strike: Quantity,
        volume: Quantity,
        callPut: CallOrPut
        ) extends SpreadOption(market, period, strike, volume, callPut) with MultiLeg {
  assert(!spreads.isEmpty, "Period is not a valid CSO period: " + period)

  override def expiryDay = Some(market.csoOptionExpiry(spreads.last))

  def valuationCCY = strike.numeratorUOM

  def explanation(env : Environment) : NamedQuantity = {
    val subExplanations : List[NamedQuantity] = childCSOs.map{cso =>
      val csoExp = cso.explanation(env)
      SimpleNamedQuantity(cso.period.toString + " value", csoExp)
    }
    FunctionNamedQuantity("Sum", subExplanations, subExplanations.map(_.quantity).sum)
  }

  private def childCSOs = spreads.map{s => SingleCalendarSpreadOption(market, market.csoOptionExpiry(s), s.first, s.last, strike, volume, callPut)}

  def spreads: List[Spread[Month]] = period match {
    case SpreadPeriod(front: Month, back: Month) => List(Spread(front, back))
    case StripPeriod(first: SpreadPeriod, last: SpreadPeriod) => {
      Strip(first, last).toList.map {
        case SpreadPeriod(front: Month, back: Month) => Spread(front, back)
      }
    }
    case DateRangePeriod(period) if period.canSplitIntoMonths => { // date ranges are always monthly
      period.toListOfMonths.map(m => Spread(m, m + 1))
    }
    case _ => throw new Exception("Invalid period for CSO: " + period)
  }

  def asUtpPortfolio(tradeDay:Day) = UTP_Portfolio({
    childCSOs.map{cso => cso.copy(volume = cso.volume.copy(value = 1.0)) -> cso.volume.value}.toMap
  })

  def legs = spreads.map(p => CalendarSpreadOption(market, p, strike, volume, callPut))

  def tradeableType = CalendarSpreadOption

  override def fixUpMyCashInstruments(ci: CashInstrument) = {
    ci.copy(index = Some(Right(market)), averagingPeriod = Some(period))
  }
}

case class SingleCalendarSpreadOption(
        market: FuturesMarket,
        override val exerciseDay: Day,
        firstMonth: Month,
        secondMonth: Month,
        strike: Quantity,
        volume: Quantity,
        callPut: CallOrPut
        ) extends SingleSpreadOption(market, exerciseDay, Spread(firstMonth, secondMonth), strike, volume, callPut) {
  def instrumentType = CalendarSpreadOption

  def explanation(env : Environment) : NamedQuantity = {
    val (_, (undiscountedCSOPrice, spreadPrice, stdDev, discount, time)) = priceWithExplanationDetails(env.withNaming())
    FunctionNamedQuantity("CSO-" + callPut, List(spreadPrice, stdDev, time, strike.named("K")), undiscountedCSOPrice) * volume.named("Volume") * discount
  }
  val valuationCCY = strike.uom * market.uom

  override def forwardState(env: Environment, dayAndTime: DayAndTime) = {
    if (exerciseDay.endOfDay > dayAndTime) {
      // exercise day after the forward day, still not expired
      this
    } else if (env.marketDay >= exerciseDay.endOfDay) {
      // already expired option, do nothing
      this
    } else {
      val exerciseDayEnv = env.forwardState(exerciseDay.endOfDay)
      val callPutSign = callPut match {case Call => 1.0; case Put => -1.0}
      val spread = exerciseDayEnv.spreadPrice(market, period)
      callPut.payoff(spread, strike) match {
        case Some(p) => new FuturesCalendarSpread(market, firstMonth, secondMonth, strike, volume * callPutSign)
        case None => this
      }
    }
  }

  def periods = List(firstMonth, secondMonth)

  def * (scale : Double) = copy(volume = volume * scale)

  override def priceAndVolKeys(marketDay : DayAndTime) = {
    var (pk, vk) = super.priceAndVolKeys(marketDay)
    pk = if (pk.isEmpty) pk else Set(FuturesSpreadPrice(market, period))
    (pk, vk)
  }

  def periodKey = Some(SpreadPeriod(firstMonth, secondMonth))
  override def riskMarketExtra = String.format("%6.2f%n ", new java.lang.Double(strike.value)) + callPut.toShortString
  override def atomicKeyCachingUTP : UTP = copy(strike = 1.0(market.priceUOM))
  override def interpolatedVol(env : Environment, volKey : EnvironmentDifferentiable with VolKey) : Quantity = {
    volKey match {
      case SpreadAtmStdDevAtomicDatumKey(`market`, SpreadPeriod(`firstMonth`, `secondMonth`), _) => env.spreadStdDev(market, Spread(firstMonth, secondMonth), exerciseDay, strike)
      case _ => throw new Exception("Unexpected vol key " + volKey)
    }
  }

  def price(env : Environment) = priceWithExplanationDetails(env)._1

  private def priceWithExplanationDetails(env : Environment) : (Quantity, (NamedQuantity, NamedQuantity, NamedQuantity, NamedQuantity, NamedQuantity)) = {
    if (isLive(env.marketDay)) {
      val marketDay = env.marketDay.day
      val T = exerciseDay.endOfDay.timeSince(env.marketDay)
      val S = env.spreadPrice(market, period)
      val K = strike
      val discount = env.discount(valuationCCY, exerciseDay)
      val (undiscountedPrice,  stdDev) = if (T == 0) {
        (callPut.intrinsicPrice(S, K), Quantity(0.0, market.priceUOM))
      } else {
        val annualisedStdDev = env.spreadStdDev(market, Spread(firstMonth, secondMonth), exerciseDay, strike)
        val undiscountedPriceValue = new SpreadOptionCalculations(callPut, S.value, K.value, annualisedStdDev.checkedValue(market.priceUOM), 0.0, T).undiscountedPrice
        val undiscountedPrice = Quantity(undiscountedPriceValue, market.priceUOM)
        (undiscountedPrice, annualisedStdDev)
      }
      (undiscountedPrice * discount, (undiscountedPrice.named("Undiscounted CSO price"), S.named("Spread Price"), stdDev.named("Std Dev"), discount.named("Discount"), new Quantity(T).named("T")))
    } else {
      (0.0 (market.priceUOM), (NamedQuantity.NULL, NamedQuantity.NULL, NamedQuantity.NULL, NamedQuantity.NULL, NamedQuantity.NULL))
    }
  }
}

object CalendarSpreadOption extends InstrumentType[SingleCalendarSpreadOption] with TradeableType[CalendarSpreadOption] {
  val name = "Calendar Spread Option"

  def createTradeable(row: RichInstrumentResultSetRow) = {
    create(row, row.getQuantity("Quantity"))
  }

  def create(row: RichInstrumentResultSetRow, volume: Quantity) = {
    val period = row.getPeriod("Period")
    new CalendarSpreadOption(row.getFuturesMarket("Market"), period, row.getQuantity("Strike"), volume, row.getCallPut("CallPut"))
  }

  def sample = {
    import starling.quantity.UOM._
    new CalendarSpreadOption(Market.NYMEX_WTI, Spread(Month(2010, 10), Month(2010, 11)), Quantity(0.2, USD / BBL), Quantity(10000, BBL), Call)
  }
}
