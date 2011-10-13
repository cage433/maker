package starling.instrument

import starling.calendar.BusinessCalendar
import java.sql.ResultSet
import starling.quantity.Quantity._
import starling.utils.ImplicitConversions._
import starling.richdb.RichInstrumentResultSetRow
import starling.curves._
import starling.market._
import rules._
import starling.market.formula._
import starling.daterange._
import starling.daterange.DateRangePeriod
import starling.models.{DefaultValuationParameters, DefaultRiskParameters}
import starling.quantity._

/**A class which represents a swap against some index - typically a front futures price
 *
 * The period can be a daterange or a strip. If it is a daterange a single swap is created, if it's a strip
 * a swap with the same volume is created for each daterange in the strip.
 * 
 * If the swap is 'cleared' it means it is margined and there is no discounting.
 */
case class CommoditySwap(
  index: Index,
  strike: Quantity,
  _volume: Quantity,
  averagingPeriod: Period,
  cleared: Boolean,
  pricingRule: SwapPricingRule = NoPricingRule,
  roundingMethodRule: RoundingMethodRule = PerQuoteRule
)
  extends Swap(index, strike, _volume, averagingPeriod, cleared, pricingRule, roundingMethodRule) with MultiLeg
{

  require(index.convert(_volume, strike.denominatorUOM).isDefined, "Couldn't convert volume into strike uom: " + (_volume, strike) + ", " + index)
  require(pricingRule.isValid(index.calendars), "Invalid pricing rule for " + index)

  def subPeriodSwaps : List[SinglePeriodSwap] = {
    dateRanges.map(SinglePeriodSwap(index, roundedStrike, volume, _, cleared, pricingRule))
  }

  def explanation(env : Environment) : NamedQuantity = {
    val subExplanations : List[NamedQuantity] = subPeriodSwaps.map{swap =>
      val swapExp = swap.explanation(env)
      SimpleNamedQuantity(swap.period.toShortString + " value", swapExp)
    }
    FunctionNamedQuantity("Sum", subExplanations, subExplanations.map(_.quantity).sum)
  }

  // volume converted so everything is in the context of the strike uom
  val volume = index.convert(_volume, strike.denominatorUOM).get

  private def priceRounding = CommoditySwap.priceRounding(index, cleared)

  /**
   * In aspect the strike (or price) is rounded before multiplying it by the volume in order to work out
   * the fixed part. Aspect thinks you can specify different rounding for the fixed and floating parts
   * of the trade but because of limitations in EAI you can't.
   * So this is wrong, and recognised as wrong, but we want to copy it to match Aspect.
   */
  def roundedStrike = priceRounding.map(strike.round).getOrElse(strike)

  private val dateRanges: List[DateRange] = averagingPeriod.toList

  override def expiryDay() = Some(dateRanges.sorted.last.lastDay)

  def asUtpPortfolio(tradeDay: Day) = {
    val swapMap : Map[UTP, Double] = subPeriodSwaps.map{swap => swap.copy(strike = Quantity(0, swap.strike.uom), volume = Quantity(1.0, swap.volume.uom)) -> swap.volume.value}.toMap

    val cashMap = dateRanges.map {
      case subPeriod => {
        val cash = if (!cleared) {
          CashInstrument(
            CashInstrumentType.General, Quantity(1.0, valuationCCY), cashSettlementDay(subPeriod.lastDay),
            Some(Left(index)), Some(DateRangePeriod(subPeriod)))
        } else {
          BankAccount(1.0(valuationCCY), None, Some(index), subPeriod)
        }
        cash -> (-roundedStrike * volume).checkedValue(valuationCCY)
      }
    }.toMap

    UTP_Portfolio(swapMap ++ cashMap)
  }

  def tradeableType = CommoditySwap

  protected def cashSettlementDay(subPeriod: DateRange) = CommoditySwap.swapSettlementDate(subPeriod.lastDay)

  def legs = dateRanges.map(p => copy(averagingPeriod = p))
}

case class SinglePeriodSwap(
  index: Index,
  strike: Quantity,
  volume: Quantity,
  period: DateRange,
  cleared: Boolean,
  pricingRule: SwapPricingRule = NoPricingRule,
  settlementDayOption : Option[Day] = None,
  roundingMethodRule: RoundingMethodRule = PerQuoteRule
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

  override def priceAndVolKeys(marketDay : DayAndTime) = {
    var (pk, _) = super.priceAndVolKeys(marketDay)
    pk = if (pk.isEmpty)
      pk
    else {
      def singleIndices(index : Index) : Set[SingleIndex] = index match {
        case si : SingleIndex => Set(si)
        case mi : MultiIndex => mi.indexes.flatMap(singleIndices(_))
      }
      singleIndices(index).flatMap{
        si =>
          val periods = liveAveragingDays(marketDay).filter(si.isObservationDay)
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

  override def priceRounding = CommoditySwap.priceRounding(index, cleared)

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

object CommoditySwap extends InstrumentType[SinglePeriodSwap] with TradeableType[CommoditySwap] {
  val name = "Commodity Swap"

  def priceRounding(index: Index, cleared: Boolean) = index.precision.map {
    case Precision(defaultRounding, clearportRounding) => {
      if (cleared) {
        clearportRounding
      } else {
        defaultRounding
      }
    }
  }

  /**
   * The default settlement date for swaps is (I believe) generally the
   * fifth business day of the following month.
   */
  // TODO [07 Jan 2010] check this, although the consequences of getting it wrong
  // TODO [07 Jan 2010]  shouldn't be dire. Mtm will just have a slightly incorrect discount
  def swapSettlementDate(dayInSwap: Day): Day = {
    dayInSwap.containingMonth.lastDay.addBusinessDays(BusinessCalendar.NONE, 5)
  }

  def createTradeable(row: RichInstrumentResultSetRow) = {
    val index = row.getIndexFromName("Market")
    CommoditySwap(index, row.getQuantity("InitialPrice"), row.getQuantity("Quantity"),
      row.getPeriod("Period"), row.getBoolean("Cleared"), row.getSwapPricingRule("PricingRule"),
      row.getRoundingMethodRule("RoundingMethodRule"))
  }

  def sample: CommoditySwap = {
    import starling.quantity.Quantity._
    import starling.quantity.UOM._
    CommoditySwap(Index.WTI10, 123(USD / BBL), 77000(BBL), Month(2015, 1), true, CommonPricingRule)
  }
}
