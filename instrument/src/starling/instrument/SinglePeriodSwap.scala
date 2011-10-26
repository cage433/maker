package starling.instrument

import starling.market.rules.{PerQuoteRule, RoundingMethodRule, NoPricingRule, SwapPricingRule}
import starling.market.{MultiIndex, SingleIndex, Index}
import starling.curves.SwapPrice._
import starling.daterange.DateRangePeriod._
import starling.daterange.DateRange._
import starling.quantity.Quantity._
import starling.quantity.SimpleNamedQuantity._
import starling.daterange.{DateRangePeriod, DayAndTime, Day, DateRange}
import starling.curves.{SwapPrice, Environment}
import starling.quantity.{SimpleNamedQuantity, NamedQuantity, UOM, Quantity}

case class SinglePeriodSwap(
  index: Index,
  strike: Quantity,
  volume: Quantity,
  period: DateRange,
  cleared: Boolean,
  pricingRule: SwapPricingRule = NoPricingRule,
  roundingMethodRule: RoundingMethodRule = PerQuoteRule,
  roundingOverride: Option[Int] = None,
  settlementDayOption : Option[Day] = None
)
  extends UTP
{
  assert(pricingRule.isValid(index.calendars), "Invalid pricing rule for " + index)
  require(index.convert(volume, strike.denominatorUOM).isDefined, "Couldn't convert volume into strike uom: " + (volume, strike) + ", " + index)

  // volume converted so everything is in the context of the strike uom
  val convertedVolume = index.convert(volume, strike.denominatorUOM).get

  def valuationCCY: UOM = strike.numeratorUOM.inBaseCurrency

  private val averagingDays = period.days.filter(pricingRule.isObservationDay(index.calendars, _))

  def liveAveragingDays(marketDay : DayAndTime) = averagingDays.filter(_.endOfDay > marketDay)

  def daysForPositionReport(marketDay : DayAndTime) : Seq[Day] = {
    val live = liveAveragingDays(marketDay)
    if (live.isEmpty)
      List(averagingDays.last)
    else
      live
  }

  override def priceAndVolKeys(env: Environment) = {
    var (pk, _) = super.priceAndVolKeys(env)
    pk = if (pk.isEmpty)
      pk
    else {
      def singleIndices(index : Index) : Set[SingleIndex] = index match {
        case si : SingleIndex => Set(si)
        case mi : MultiIndex => mi.indexes.flatMap(singleIndices(_))
      }
      singleIndices(index).flatMap{
        si =>
          val periods = liveAveragingDays(env.marketDay).filter(si.isObservationDay)
          periods.map(SwapPrice(si, _))
      }.toSet
    }
    (pk, Set())
  }

  def periodKey = Some(DateRangePeriod(period))

  /**
   * The price uom can be different from the underlying index
   * */
  def priceUOM = strike.uom

  def price(env : Environment) = {
    // According to Jon fox this should be the remaining price (i.e. not including what has already fixed)
    val bom = liveAveragingDays(env.marketDay)
    if(!bom.isEmpty) {
      val bomPeriod = DateRange(bom.head, bom.last)
      env.averagePrice(index, bomPeriod, pricingRule, priceUOM, priceRounding, roundingMethodRule)
    } else {
      Quantity(0, priceUOM)
    }
  }

  def explanation(env : Environment) : NamedQuantity = {
    val namedEnv = env.withNaming()
    val price = SimpleNamedQuantity("F_Avg", namedEnv.averagePrice(index, period, pricingRule, priceUOM, priceRounding, roundingMethodRule))
    val undiscounted = (price - strike.named("K")) * convertedVolume.named("Volume")
    if(cleared) {
      undiscounted
    } else {
      val discount = SimpleNamedQuantity("disc", namedEnv.discount(valuationCCY, settlementDay))
      undiscounted * discount
    }
  }

  def instrumentType = CommoditySwap

  def *(x: Double) = copy(volume = volume * x)

  val settlementDay = settlementDayOption.getOrElse(CommoditySwap.swapSettlementDate(period.lastDay))

  override def priceRounding = CommoditySwap.priceRounding(index, cleared, roundingOverride)

  def assets(env: Environment) = {
    val assets = {
      val days = pricingRule.observationDays(index.calendars, period)
      if (days.isEmpty) {
        List()
      } else {
        val price = env.averagePrice(index, period, pricingRule, priceUOM, priceRounding, roundingMethodRule)

        val payment = (price - strike) * convertedVolume
        if (env.marketDay < days.last.endOfDay) {
          if(cleared) { // cleared means we're margining so no discounting
            List(Asset.estimatedCash(env.marketDay.day, payment, payment))
          } else {
            List(Asset.estimatedCash(settlementDay, payment, env))
          }
        } else {
          if (cleared) {
            // cleared means we're margining so no discounting
            List(Asset.estimatedCash(env.marketDay.day, payment, payment))
          } else {
            List(Asset.knownCash(settlementDay, payment, env))
          }
        }
      }
    }
    Assets(assets)
  }

  override def knownConversion = Some(index)
}
