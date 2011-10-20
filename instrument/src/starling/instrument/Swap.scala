package starling.instrument

import starling.calendar.BusinessCalendar
import java.sql.ResultSet
import starling.quantity.{Quantity, UOM, Percentage}
import starling.quantity.Quantity._
import starling.utils.ImplicitConversions._
import starling.richdb.RichInstrumentResultSetRow
import starling.curves._
import starling.market._
import rules._
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
                     pricingRule: SwapPricingRule = NoPricingRule,
                     roundingMethodRule: RoundingMethodRule = PerQuoteRule,
                     roundingOverride: Option[Int] = None
                     )
  extends Tradeable {

  def isLive(dayAndTime: DayAndTime): Boolean = dayAndTime < expiryDay.get.endOfDay

  def persistedTradeableDetails :Map[String, Any] = Map(
    "Market" -> index,
    "Period" -> averagingPeriod,
    "Initial Price" -> strike,
    "Quantity" -> volume,
    "Cleared" -> cleared,
    "PricingRule" -> pricingRule,
    "RoundingMethodRule" -> roundingMethodRule,
    "RoundingOverride" -> roundingOverride
  )

  def valuationCCY: UOM = strike.numeratorUOM.inBaseCurrency

  override def fixUpMyCashInstruments(ci: CashInstrument) = {
    // sometimes the cash instrument has already been assigned to a market and period and we don't want to override that here
    if(ci.index.isEmpty && ci.averagingPeriod.isEmpty)
      ci.copy(index = Some(Left(index)), averagingPeriod = Some(averagingPeriod))
    else
      ci
  }
}