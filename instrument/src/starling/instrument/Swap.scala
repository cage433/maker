package starling.instrument

import starling.calendar.BusinessCalendar
import java.sql.ResultSet
import starling.quantity.{Quantity, UOM, Percentage}
import starling.quantity.Quantity._
import starling.utils.ImplicitConversions._
import starling.richdb.RichInstrumentResultSetRow
import starling.curves._
import starling.market._
import rules.{NoPricingRule, CommonPricingRule, SwapPricingRule}
import starling.daterange._
import starling.daterange.DateRangePeriod

/**A class which represents a swap against some index - typically a front futures price
 *
 * If the swap is 'cleared' it means it is margined and there is no discounting.
 */
abstract class Swap(
                     index: Index,
                     strike: Quantity,
                     volume: Quantity,
                     averagingPeriod: Period,
                     cleared: Boolean,
                     pricingRule: SwapPricingRule = NoPricingRule
                     )
	extends Tradeable
{
  def isLive(dayAndTime: DayAndTime): Boolean = dayAndTime < expiryDay.get.endOfDay

  def tradeableDetails :Map[String, Any] = Map(
    "Market" -> index,
    "Period" -> averagingPeriod,
    "Initial Price" -> strike,
    "Quantity" -> volume,
    "Cleared" -> cleared,
    "PricingRule" -> pricingRule)

  def valuationCCY: UOM = strike.numeratorUOM

  override def fixUpMyCashInstruments(ci: CashInstrument) = {
    // sometimes the cash instrument has already been assigned to a market and period and we don't want to override that here
    if(ci.index.isEmpty && ci.averagingPeriod.isEmpty)
      ci.copy(index = Some(Left(index)), averagingPeriod = Some(averagingPeriod))
    else
      ci
  }
}

abstract class SingleSwap(
        index: Index,
        strike: Quantity,
        volume: Quantity,
        val averagingPeriod: DateRange,
        val cleared: Boolean,
        pricingRule: SwapPricingRule = NoPricingRule
        )
        extends UTP 
{
  assert(pricingRule.isValid(index.calendars), "Invalid pricing rule for " + index)
  assert(strike.denominatorUOM == volume.uom, "Price and volume different: " + (strike, volume))

  def valuationCCY: UOM = strike.numeratorUOM

  protected def averagingDays = averagingPeriod.days.filter(pricingRule.isObservationDay(index.calendars, _))

  def liveAveragingDays(marketDay : DayAndTime) = averagingDays.filter(_.endOfDay > marketDay)

  private def liveAveragingDays(marketDay : DayAndTime, si: SingleIndex) = averagingDays.filter(_.endOfDay > marketDay).filter(si.isObservationDay(_))

  def details: Map[String, Any] = Map(
    "Market" -> index,
    "Period" -> averagingPeriod,
    "Strike" -> strike,
    "Cleared" -> cleared,
    "PricingRule" -> pricingRule)

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

  def periodKey = Some(DateRangePeriod(averagingPeriod))

  /**
   * The price uom can be different from the underlying index
   * */
  def priceUOM = strike.uom

  def price(env : Environment) = {
    // According to Jon fox this should be the remaining price (i.e. not including what has already fixed)
    val bom = liveAveragingDays(env.marketDay)
    if(!bom.isEmpty) {
      val bomPeriod = DateRange(bom.head, bom.last)
      env.averagePrice(index, bomPeriod, pricingRule, priceUOM)
    } else {
      Quantity(0, priceUOM)
    }
  }
}
