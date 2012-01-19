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
import starling.quantity.UOM._
import scalaz.Scalaz._
import starling.quantity.{Percentage, NamedQuantity, UOM, Quantity}

/**
 * A Forward Freight Agreement - basically a swap
 *
 * (avg(index) * worldscale_flatrate_floating_year - fixed * worldscale_flatrate_fixed_year) * V
 */
case class FFA(
                index: SingleIndex,
                volume: Quantity,
                fixed: Percentage,
                averagingPeriod: DateRange,
                fixedLegFlatRateYear: Year,
                fixedLegFlatRate: Option[Quantity],
                floatingLegFlatRateYear: Year,
                roundingOverride: Option[Int] = None
                )
  extends UTP with Tradeable {
  fixedLegFlatRate.map(q => assert(q.uom == USD/MT, "Worldscale value should be defined in USD/MT: " + q))

  def isLive(dayAndTime: DayAndTime): Boolean = dayAndTime < expiryDay.get.endOfDay

  def persistedTradeableDetails: Map[String, Any] = Map(
    "Market" -> index,
    "Period" -> averagingPeriod,
    "Fixed Rate" -> fixed,
    "Quantity" -> volume,
    "fixedLegFlatRateYear" -> fixedLegFlatRateYear,
    "fixedLegFlatRate" -> fixedLegFlatRate,
    "floatingLegFlatRateYear" -> floatingLegFlatRateYear,
    "RoundingOverride" -> roundingOverride
  )

  def valuationCCY: UOM = USD

  def priceUOM = USD / MT

  override def fixUpMyCashInstruments(ci: CashInstrument) = {
    // sometimes the cash instrument has already been assigned to a market and period and we don't want to override that here
    if (ci.index.isEmpty && ci.averagingPeriod.isEmpty)
      ci.copy(index = Some(Left(index)), averagingPeriod = Some(averagingPeriod))
    else
      ci
  }

  def explanation(env: Environment): NamedQuantity = {
    value(env.withNaming(), {
      case (q: Quantity, s: String) => q.named(s)
    }).asInstanceOf[NamedQuantity]
  }

  def assets(env: Environment) = {
    val payment = value(env, {
      case (q: Quantity, s: String) => q
    })
    Assets(Asset.estimatedCash(env.marketDay.day, payment, payment))
  }

  private def value(env: Environment, named: ((Quantity, String) => Quantity)): Quantity = {
    val floatingRate = named(env.worldScaleFlatRate(index.market, floatingLegFlatRateYear), "Floating WSC " + floatingLegFlatRateYear)
    val averagePrice = env.averagePrice(index, averagingPeriod, None)
    val averagePriceScaled = averagePrice * floatingRate
    val F = named(priceRounding.fold(averagePriceScaled.round, averagePriceScaled), "F")

    val fixedQ = fixed.toQuantity
    val fixedQScaled = fixedLegFlatRate match {
      case Some(r) => named(fixedQ, "Fixed") * named(r, "Fixed Rate")
      case _ => named(fixedQ, "Fixed") * named(env.worldScaleFlatRate(index.market, fixedLegFlatRateYear), "Fixed WSC " + fixedLegFlatRateYear)
    }
    val K = named(priceRounding.fold(fixedQScaled.round, fixedQScaled), "K")
    val V = named(volume, "V")
    (F - K) * V
  }

  def price(env: Environment) = {
    val bom = liveAveragingDays(env.marketDay)
    if (!bom.isEmpty) {
      val bomPeriod = DateRange(bom.head, bom.last)
      val avg = env.averagePrice(index, bomPeriod, priceRounding)
      val floatingRate = env.worldScaleFlatRate(index.market, floatingLegFlatRateYear)
      avg * floatingRate
    } else {
      Quantity(0, priceUOM)
    }
  }

  override def hedgingInstrument(env:Environment, diff:EnvironmentDifferentiable) = diff match {
    case PriceDifferentiable(market:CommodityMarket, period) if Index.getPublishedIndexForMarket(market).isDefined => {
      val index = Index.getPublishedIndexForMarket(market).get
      assert(index == this.index, "Wrong index for this: " + (index, this))
      Some(this.copy(volume = Quantity(1, volume.uom), fixed = Percentage(0.0)))
    }
    case _ => None
  }

  override def priceRounding = FFA.priceRounding(index, roundingOverride)

  override def expiryDay() = Some(averagingPeriod.lastDay)

  def asUtpPortfolio(tradeDay: Day) = UTP_Portfolio(Map(this -> 1.0))

  def tradeableType = FFA

  def instrumentType = FFA

  private val averagingDays = averagingPeriod.days.filter(index.isObservationDay)

  def liveAveragingDays(marketDay: DayAndTime) = averagingDays.filter(_.endOfDay > marketDay)

  def daysForPositionReport(marketDay: DayAndTime): Seq[Day] = {
    val live = liveAveragingDays(marketDay)
    if (live.isEmpty)
      List(averagingDays.last)
    else
      live
  }

  def *(scale: Double) = copy(volume = volume * scale)

  def periodKey = Some(averagingPeriod)
}

object FFA extends InstrumentType[FFA] with TradeableType[FFA] {
  val name = "FFA"

  def priceRounding(index: Index, roundingOverride: Option[Int]): Option[Int] = {
    roundingOverride.fold(r => Some(r), index.precision.fold(p => Some(p.default), throw new Exception("Precision not defined for " + index)))
  }

  def createTradeable(row: RichInstrumentResultSetRow) = {
    val index = row.getSingleIndexFromName("Market")
    val fixedLegFlatRateYear = row.getDateRange("fixedLegFlatRateYear", Some(Year)).asInstanceOf[Year]
    val floatingLegFlatRateYear = row.getDateRange("floatingLegFlatRateYear", Some(Year)).asInstanceOf[Year]
    FFA(index, row.getQuantity("Quantity"), row.getPercentage("FixedRate"),
      row.getDateRange("Period"), fixedLegFlatRateYear,
      row.getQuantityOption("fixedLegFlatRate"),
      floatingLegFlatRateYear,
      row.getIntOption("RoundingOverride"))
  }

  def sample: FFA = {
    import starling.quantity.Quantity._

    val volume: Quantity = 10000.0 (MT)
    FFA(Index.TC6_CROSS_MEDITERRANEAN_30KT_BALTIC, volume, Percentage.fromPercentage(143), Month(2011, 1), Year(2011), None, Year(2011))
  }
}
