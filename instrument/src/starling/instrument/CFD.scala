package starling.instrument

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
import starling.calendar.{BrentMonth, BusinessCalendar}

/**
 * A CFD is a swap specifically between Dated Brent and a future Platts index.
 */
case class CFD(index: MultiIndex,
               strike: Quantity,
               volume: Quantity,
               period: DateRange,
               pricingRule: SwapPricingRule = CommonPricingRule)
  extends Swap(index, strike, volume, DateRangePeriod(period), true, pricingRule) {

  override def expiryDay() = Some(period.lastDay)

  def asUtpPortfolio(tradeDay: Day) = {
    var map = Map.empty[UTP, Double]
    map += SingleCFD(index, Quantity(0, strike.uom), new Quantity(1.0, volume.uom), period, pricingRule) -> volume.value
    map += BankAccount(1.0(valuationCCY), None, Some(index), period) -> (-strike * volume).value
    UTP_Portfolio(map)
  }

  def tradeableType = CFD
}

case class SingleCFD(index: Index,
                     strike: Quantity,
                     volume: Quantity,
                     period: DateRange,
                     pricingRule: SwapPricingRule = CommonPricingRule)
  extends SingleSwap(index, strike, volume, period, true, pricingRule) {

  def instrumentType = CFD

  def *(x: Double) = copy(volume = volume * x)

  def assets(env: Environment) = {
    val assets = {
      val days = pricingRule.observationDays(index.markets, averagingPeriod)
      if (days.isEmpty) {
        List()
      } else {
        val price = env.averagePrice(index, averagingPeriod, pricingRule, priceUOM)
        val payment = (price - strike) * volume
        val marketDay = env.marketDay.day
        if (env.marketDay < days.last.endOfDay) {
          List(Asset.estimatedCash(marketDay, payment, payment))
        } else {
          List(Asset.knownCash(marketDay, payment, env))
        }
      }
    }
    Assets(assets)
  }
}

object CFD extends InstrumentType[SingleCFD] with TradeableType[CFD] {
  val name = "CFD"

  def createTradeable(row: RichInstrumentResultSetRow) = {
    val index = row.getIndexFromName("Market").asInstanceOf[MultiIndex]
    val dateRange = row.getPeriod("Period") match {
      case DateRangePeriod(p) => p
      case u => throw new Exception("Unrecognised period for CFD: " + u)
    }
    CFD(index, row.getQuantity("InitialPrice"), row.getQuantity("Quantity"), dateRange, row.getSwapPricingRule("PricingRule"))
  }

  def sample = {
    import starling.quantity.Quantity._
    import starling.quantity.UOM._
    CFD(BrentCFDSpreadIndex(PublishedIndex.PLATTS_BRENT(new BrentMonth(4))), 123(USD / BBL), 77000(BBL), Quarter(2015, 1), CommonPricingRule)
  }
}
