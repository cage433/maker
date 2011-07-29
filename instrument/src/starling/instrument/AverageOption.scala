package starling.instrument

import starling.richdb.RichInstrumentResultSetRow
import starling.quantity.Quantity._
import java.sql.ResultSet
import starling.utils.CollectionUtils._
import starling.utils.ImplicitConversions._
import starling.daterange._
import starling.quantity.Percentage._
import starling.curves._
import starling.market._
import starling.models._
import starling.utils.MathUtil
import starling.daterange.DateRangePeriod
import starling.quantity._

abstract class AverageOption(
        index: SingleIndex,
        averagingPeriod: Period,
        strike: Quantity,
        volume: Quantity,
        callPut: CallOrPut
        ) extends Tradeable {
  def persistedTradeableDetails: Map[String, Any] = Map("Market" -> index,
    "Period" -> averagingPeriod,
    "Strike" -> strike, "Quantity" -> volume, "CallPut" -> callPut)

  val periods = averagingPeriod match {
    case DateRangePeriod(p) => List(p)
    case StripPeriod(first, last) => (first, last) match {
      case (f: DateRangePeriod, l: DateRangePeriod) => {
        assert(f.period.canSplitIntoMonths, "Must be able to split period into months: " + f)
        assert(l.period.canSplitIntoMonths, "Must be able to split period into months: " + l)
        f.period.toListOfMonths.head upto l.period.toListOfMonths.last
      }
    }
  }

}

abstract class SingleAverageOption(
        index: SingleIndex,
        val averagingPeriod: DateRange,
        strike: Quantity,
        volume: Quantity,
        callPut: CallOrPut
        ) extends UTP {
  def detailsForUTPNOTUSED: Map[String, Any] = Map("Market" -> index,
    "Period" -> averagingPeriod,
    "Strike" -> strike, "CallPut" -> callPut)

  def explanation(env : Environment) : NamedQuantity = {
    val namedEnv = env.withNaming()
    val (undiscPrc, (avePrice, vol)) = undiscountedPrice(namedEnv)
    val discount = namedEnv.discount(valuationCCY, settlementDate)
    FunctionNamedQuantity("Curran-" + callPut, List(avePrice, vol, strike.named("K")), undiscPrc) * volume.named("Volume") * discount.named("Discount")
  }

  val averagingDays: List[Day]

  def liveAveragingDays(marketDay : DayAndTime) = averagingDays.filter(_.endOfDay > marketDay)

  val settlementDate: Day

  def expiryDay: Day = averagingDays.last

  def assets(env: Environment) = if (env.marketDay < averagingDays.last.endOfDay) {
    Assets(Asset.estimatedCash(settlementDate, undiscountedMtm(env), env))
  } else {
    val fixedDays = averagingDays.filter(_.endOfDay <= env.marketDay)
    val price = env.averagePrice(index, fixedDays.head upto fixedDays.last)
    callPut.payoff(price, strike) match {
      case Some(p) => Assets(Asset.knownCash(settlementDate, p * volume, env))
      case None => Assets()
    }
  }

  private def undiscountedMtm(env: Environment): Quantity = {
    undiscountedPrice(env)._1 * volume
  }

  def valuationCCY = strike.numeratorUOM

  def daysForPositionReport(marketDay : DayAndTime) : Seq[Day] = {
    val liveAveragingDays = averagingDays.filter(_.endOfDay > marketDay)
    if (liveAveragingDays.isEmpty)
      List(averagingDays.last)
    else
      liveAveragingDays
  }

  override def priceAndVolKeys(marketDay : DayAndTime) = {
    val days = averagingDays.filter(_.endOfDay > marketDay).toSet
    val priceKeys : Set[EnvironmentDifferentiable with PriceKey] = days.map(SwapPrice(index, _))
    val volKeys : Set[EnvironmentDifferentiable with VolKey] = days.map(SwapVol(index, _))
    (priceKeys, volKeys)
  }

  def periodKey = Some(DateRangePeriod(averagingPeriod))

  override def interpolatedVol(env : Environment, volKey : EnvironmentDifferentiable with VolKey) : Quantity = {
    def futuresVol(market : CommodityMarket, period : DateRange) : Quantity = {
      index match {
        case f : FuturesFrontPeriodIndex => {
          assert(f.market == market, "Unexpected vol key " + volKey)
          env.impliedVol(market, period, period.lastDay, strike)
        }
        case `index` => {
          env.swapVol(index, period, strike)
        }
        case _ => throw new Exception("Unexpected vol key " + volKey)
      }
    }
    volKey match {
      case BradyMetalVolAtomicDatumKey(market, period) => futuresVol(market, period)
      case OilAtmVolAtomicDatumKey(market, _, period, _) => futuresVol(market, period)
      case SwapVol(`index`, period) => env.swapVol(index, period, strike)
      case _ => throw new Exception("Unexpected vol key " + volKey)
    }
  }

  def undiscountedPrice(env : Environment) : (Quantity, (NamedQuantity, NamedQuantity)) = {
    val pricingDays = averagingDays
    val marketDay = env.marketDay
    val daysInFuture = pricingDays.filter(_.endOfDay > marketDay)
    val dayPriceMap = pricingDays.map(d => d.endOfDay.timeSince(marketDay) -> env.fixingOrForwardPrice(index, d)).toMap
    val unfixed = dayPriceMap.filterKeys(_ > 0.0)
    val vol = (if (daysInFuture.isEmpty) Percentage(1e-6) else env.swapVol(index, SimpleDateRange.containingRange(daysInFuture), strike))
    val prices = dayPriceMap.keySet.toList.sortWith(_<_).map(dayPriceMap)
    val avePrice = SimpleNamedQuantity("Average(" + index + "." + averagingPeriod + ")", Quantity.average(prices))
    (Quantity(Curran(callPut, dayPriceMap, strike, vol).value, strike.uom), (avePrice, PercentageNamedQuantity("Vol", vol)))
  }

  def isLive(dayAndTime: DayAndTime) = dayAndTime < averagingPeriod.lastDay.endOfDay

  def price(env : Environment) = {
    if(isLive(env.marketDay)) {
      undiscountedPrice(env)._1
    } else {
      0 (index.priceUOM)
    }
  }

  override def forwardState(env: Environment, dayAndTime: DayAndTime) = {
    if (expiryDay.endOfDay > dayAndTime) {
      // exercise day after the forward day, still not expired
      this
    } else if (env.marketDay >= expiryDay.endOfDay) {
      // already expired option, do nothing
      this
    } else {
      val exerciseDayEnv = env.forwardState(expiryDay.endOfDay)
      val prices = averagingDays.map(d => exerciseDayEnv.fixingOrForwardPrice(index, d))
      val average = Quantity.average(prices)
      val value = callPut.payoff(average, strike) match {
        case Some(v) => v * volume
        case None => new Quantity(0, index.currency)
      }
      new CashInstrument(CashInstrumentType.General, value, settlementDate, Some(Left(index)), Some(DateRangePeriod(averagingPeriod)))
    }
  }
}
