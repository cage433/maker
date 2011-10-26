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
  roundingMethodRule: RoundingMethodRule = PerQuoteRule,
  roundingOverride: Option[Int] = None
)
  extends Swap(index, strike, _volume, averagingPeriod, cleared, pricingRule, roundingMethodRule, roundingOverride) with MultiLeg
{

  require(index.convert(_volume, strike.denominatorUOM).isDefined, "Couldn't convert volume into strike uom: " + (_volume, strike) + ", " + index)
  require(pricingRule.isValid(index.calendars), "Invalid pricing rule for " + index)

  def subPeriodSwaps : List[SinglePeriodSwap] = {
    dateRanges.map(SinglePeriodSwap(index, roundedStrike, volume, _, cleared, pricingRule, roundingMethodRule, roundingOverride))
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

  private def priceRounding = CommoditySwap.priceRounding(index, cleared, roundingOverride)

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

object CommoditySwap extends InstrumentType[SinglePeriodSwap] with TradeableType[CommoditySwap] {
  val name = "Commodity Swap"

  def priceRounding(index: Index, cleared: Boolean, roundingOverride: Option[Int]): Option[Int] = roundingOverride match {
    case Some(r) => Some(r)
    case _ => {
      index.precision.map {
        case Precision(defaultRounding, clearportRounding) => {
          if (cleared) {
            clearportRounding
          } else {
            defaultRounding
          }
        }
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
      row.getRoundingMethodRule("RoundingMethodRule"),
      row.getIntOption("RoundingOverride"))
  }

  def sample: CommoditySwap = {
    import starling.quantity.Quantity._
    import starling.quantity.UOM._
    CommoditySwap(Index.WTI10, 123(USD / BBL), 77000(BBL), Month(2015, 1), true, CommonPricingRule)
  }
}
