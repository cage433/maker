package starling.curves

import org.scalatest.matchers.ShouldMatchers
import org.testng.annotations.{Test, BeforeMethod}
import starling.pivot.{Field, PivotFieldsState, PivotQuantity}
import starling.market.Market
import starling.utils.ImplicitConversions._
import org.mockito.Mockito._
import org.mockito.Matchers._
import starling.quantity.{UOM, Quantity}
import starling.daterange._
import starling.marketdata._
import collection.immutable.{List, TreeSet, TreeMap}
import starling.utils.{ClosureUtil, StarlingTest}
import CurveViewerFields._
import starling.db.MarketDataReader


class PriceCurveViewerOutputsPivotTableDataSourceTests extends StarlingTest with ShouldMatchers {

  var pivot:PriceCurveViewerOutputsPivotTableDataSource = _

  val observationTimeOfDay = ObservationTimeOfDay.LMEClose
  var rule:EnvironmentRule = _
  var marketDataReader:MarketDataReader = _

  def environmentDay = Day(2010, 1, 1)
  def markets = List(
    UnderlyingDeliveryPeriods(observationPoint.timeOfDay, Market.LME_LEAD, TreeSet(Day(2010, 1, 1), Day(2010, 1, 2), lastDay)),
    UnderlyingDeliveryPeriods(observationPoint.timeOfDay, Market.LME_TIN, TreeSet(Day(2010, 1, 2), Day(2010, 1, 2), lastDay)),
    UnderlyingDeliveryPeriods(observationPoint.timeOfDay, Market.LME_ZINC, TreeSet(Day(2010, 1, 2), Day(2010, 1, 2), lastDay))
  )


  @BeforeMethod
  def init() {
    try {
      rule = mock(classOf[EnvironmentRule])
      marketDataReader = mock(classOf[MarketDataReader])
    } catch {
      case e => e.printStackTrace()
    }
  }

  val environmentPrice = Quantity(50, UOM.USD / UOM.MT)
  val observationPoint = ObservationPoint(environmentDay, observationTimeOfDay)
  val lastDay = Day(2010, 1, 10)
  val daysWithData = Set(Day(2010, 1, 1), Day(2010, 1, 2), lastDay)

  def env = {
    val builder = new TestEnvironmentBuilder(environmentDay.endOfDay)
    builder.setConstantPrice(Market.LME_LEAD, environmentPrice)
    builder.setConstantPrice(Market.LME_TIN, environmentPrice)
    builder.setConstantPrice(Market.LME_ZINC, environmentPrice)
    builder.build
  }


  @Test
  def shouldHaveAPriceForEveryDayForTheFollowingYearForAllMarkets {
    val envWithDomain = mock(classOf[EnvironmentWithDomain])
    when(envWithDomain.environment) thenReturn env
    when(envWithDomain.markets) thenReturn markets
    when(rule.createEnv(environmentDay, marketDataReader)) thenReturn envWithDomain
    pivot = new PriceCurveViewerOutputsPivotTableDataSource(environmentDay, rule, marketDataReader)

    val rows = pivot.data(PivotFieldsState(
      dataFields = List(priceField),
      rowFields = List(periodField),
      reportSpecificChoices = TreeMap(showInterpolatedReportOption -> true))).data

    val expectedPrices = List(CurvePrice(PivotQuantity(environmentPrice), true),
                              CurvePrice(PivotQuantity(environmentPrice), false))

    rows.mapDistinct(map => map(priceField)) should be === expectedPrices.toSet
    val expectedPeriods = (environmentDay upto lastDay).toList
    rows.mapDistinct(map => map(periodField)) should be === expectedPeriods.toSet
    val expectedMarkets = markets.map(_.market.name)
    rows.mapDistinct(map => map(marketField)) should be === expectedMarkets.toSet
  }

  @Test
  def shouldHaveAPriceForInputPriceWhenShowInterpolatedIsUnchecked {
    val rows = pivot.data(PivotFieldsState(
      dataFields = List(priceField),
      rowFields = List(periodField),
      reportSpecificChoices = TreeMap(showInterpolatedReportOption -> false))).data

    val expectedPeriods = (daysWithData.toList)
    rows.mapDistinct(map => map(periodField)) should be === expectedPeriods.toSet
  }
}