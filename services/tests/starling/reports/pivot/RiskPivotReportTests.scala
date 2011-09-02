package starling.reports.pivot

import greeks.{GreekValues, GreeksPivotReport}
import starling.utils.StarlingTest
import starling.quantity.{UOM, Percentage, Quantity}
import starling.quantity.UOM._
import org.testng.Assert._
import starling.curves._
import starling.pivot.PivotQuantity
import starling.quantity.utils.QuantityTestUtils._
import starling.models.{BlackScholes, European, Call}
import starling.daterange.{DateRange, Month, Day}
import starling.market._
import starling.varcalculator.{RiskFactorUtils, RiskFactorUtilsForTrades, VolatilityRiskFactor, ForwardPriceRiskFactor}
import starling.daterange.{DateRangePeriod, DateRange, Month, Day}
import starling.instrument.{SingleAsianOption, AsianOption, FuturesOption}
import starling.reports.pivot.PivotReport._
import starling.concurrent.MP
import org.testng.annotations.{AfterTest, AfterClass, Test}
import starling.gui.api.{ReportSpecificChoices, UTPIdentifier}

class RiskPivotReportTests extends TestMarketTest with StarlingTest{
  @Test
  def testFuturesOption {
    val market = Market.NYMEX_WTI

    val md = Day(2012, 5, 1).endOfDay
    val period = Month(2012, 10)
    val exerciseDay = market.optionExpiry(period)
    val option = new FuturesOption(market, exerciseDay, period, Quantity(100, market.priceUOM), Quantity(1000, BBL), Call, European)
    val T = exerciseDay.endOfDay.timeSince(md)

    def env = Environment(new UnitTestingAtomicEnvironment(md, {
        case _: ForwardPriceKey => Quantity(100, market.priceUOM)
        case _: OilAtmVolAtomicDatumKey => Percentage(0.4)
        case _: OilVolSkewAtomicDatumKey => Map[Double, Percentage]()
        case _: DiscountRateKey => new Quantity(scala.math.exp(-0.0 * T))
      }
    ))

    val priceKey = PriceDifferentiable(market, period)
    val volKey = OilAtmVolAtomicDatumKey(market, None, period)

    val pivotReport = new GreeksPivotReport(env, (md.day + 1).endOfDay, Map(UTPIdentifier(1) -> option))
    val rows = pivotReport.combine(pivotReport.rows(UTPIdentifier(1), option), ReportSpecificChoices(showEqFutures_str -> true))

    rows.find(r => r.gamma != PivotQuantity.NULL) match {
      case Some(d) => {
        // gamma
        val gamma = option.gamma(env, priceKey, USD, List(priceKey))
        val reportGamma = d.gamma.quantityValue.get * d.scale
        assertQtyEquals(reportGamma, gamma, 1e-5)
      }
      case _  => throw new Exception("This shouldn't happen")
    }
    rows.find(r => r.vega != PivotQuantity.NULL) match {
      case Some(d) => {
        val vega = option.vega(env, volKey)
        assertQtyEquals(d.vega.quantityValue.get * d.scale, vega, 1e-6)
        val vomma = option.vomma(env, volKey, List(volKey))
        val reportVomma = d.vomma.quantityValue.get * d.scale
        assertQtyEquals(reportVomma, vomma, 1e-6)
        assertQtyEquals(vomma, Quantity(-0.093043263572, USD), 1e-10)
      }
      case _ => throw new Exception("This shouldn't happen")
    }
  }

  @Test
  def testAsianOption {
    val market = Market.NYMEX_WTI
    val index = Index.WTI10

    val md = Day(2012, 5, 1).endOfDay
    val period = Month(2012, 10)
    val nov = Month(2012, 11)
    val dec = Month(2012, 12)
    val exerciseDay = market.optionExpiry(period)
    val option = new SingleAsianOption(index, period, Quantity(100, market.priceUOM), Quantity(1000, BBL), Call)
    val T = exerciseDay.endOfDay.timeSince(md)

    def env = Environment(new UnitTestingAtomicEnvironment(md, {
        case _: ForwardPriceKey => Quantity(100, market.priceUOM)
        case _: OilAtmVolAtomicDatumKey => Percentage(0.4)
        case _: OilVolSkewAtomicDatumKey => Map[Double, Percentage]()
        case _: DiscountRateKey => new Quantity(scala.math.exp(-0.0 * T))
      }
    ))

    val rfs = option.riskFactors(env, USD)
    val pivotReport = new GreeksPivotReport(env, (md.day + 1).endOfDay, Map(UTPIdentifier(1) -> option))
    val greeks = pivotReport.combine(pivotReport.rows(UTPIdentifier(1), option), ReportSpecificChoices(showEqFutures_str -> true))

    val gamma1 = option.gamma(env, PriceDifferentiable(market, nov), USD, List(PriceDifferentiable(market, nov), PriceDifferentiable(market, dec)))
    assertQtyClose(risk(greeks, nov, "gamma"), gamma1)
    val gamma2 = option.gamma(env, PriceDifferentiable(market, dec), USD, List(PriceDifferentiable(market, nov), PriceDifferentiable(market, dec)))
    assertQtyClose(risk(greeks, dec, "gamma"), gamma2)
  }

  def risk(greeks: List[GreekValues], dr: DateRange, riskType: String) = {
    val period = Some(DateRangePeriod(dr))
    val matches = greeks.flatMap{
      case r: GreekValues if r.period == period => riskType match {
        case "gamma" if r.gamma != PivotQuantity.NULL => Some(r.gamma * r.scale)
        case "vega" if r.vega != PivotQuantity.NULL => Some(r.vega * r.scale)
        case _ => None
      }
      case _ => None
    }
    matches match {
      case e :: Nil => e.quantityValue.get
      case _ => throw new IllegalStateException("This shouldn't happen: " + riskType + " matches: " + matches + " risks: " + greeks )
    }
  }
}
