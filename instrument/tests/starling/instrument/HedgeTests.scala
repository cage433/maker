package starling.instrument

import org.testng.annotations._
import starling.daterange.Day
import starling.utils.StarlingTest
import starling.quantity.UOM._
import starling.market.Market._
import starling.daterange.Month
import starling.quantity.Quantity
import starling.quantity.Quantity._
import starling.daterange.DayAndTime
import scala.math._
import starling.quantity.utils.QuantityTestUtils._
import starling.maths.RandomVariables
import cern.jet.random.Uniform
import starling.maths.RandomThing
import starling.utils.CollectionUtils
import org.testng.Assert._
import javax.management.remote.rmi._RMIConnection_Stub
import starling.market.{TestMarketTest, Market}
import starling.curves._

class HedgeTests extends TestMarketTest {

  def buildMonthGroups(
    u : Uniform = RandomVariables.standardUniform(12345),
    nextMonth : Month = Month(2010, 1), 
    currentGroup : List[Month] = Nil, 
    otherGroups : List[List[Month]] = Nil
  ) : List[List[Month]] = {
    if (nextMonth == Month(2013, 1))
      (currentGroup :: otherGroups).map(_.reverse).reverse.filterNot(_.isEmpty)
    else if (u.nextDouble > 0.9)
      buildMonthGroups(u, nextMonth + 1, Nil, currentGroup :: otherGroups)
    else
      buildMonthGroups(u, nextMonth + 1, nextMonth :: currentGroup, otherGroups)
  }

  @Test
  def testContiguousMonths{
    assert(Nil === Month.contiguousMonths(Nil))

    val u = RandomVariables.standardUniform(12345)
    for (i <- 0 to 10){
      val groups = buildMonthGroups(u)
        val months = groups.flatMap{list => list}
      assert(groups === Month.contiguousMonths(months))
    }
  }

  private def makeRandomFutures(months : List[Month]) : List[Future] = {
    val u = RandomVariables.standardUniform(9999)
    months.map(
      Future(
        Market.NYMEX_WTI, 
        _, 
        Quantity(0, USD/BBL), 
        Quantity((u.nextDouble - 0.5) * 20, BBL)
      )
    )
  }

  @DataProvider(name = "testFuturesSpreadHedgeDeltasAreConsistentProvider")
  def testFuturesSpreadHedgeDeltasAreConsistentProvider = {
    val monthGroups = buildMonthGroups()
    val months : List[Month] = List.concat(monthGroups : _*)
    val futures = makeRandomFutures(months)
    Array(
      Array(futures),
      Array(
        List(
          Future(NYMEX_WTI, Month(2011, 12), 100 (USD/BBL), 1000(BBL)),
          Future(NYMEX_WTI, Month(2012, 12), 99 (USD/BBL), -500.0 (BBL))))
    )
  }

  @Test(dataProvider = "testFuturesSpreadHedgeDeltasAreConsistentProvider")
  def testFuturesSpreadHedgeDeltasAreConsistent(futures : List[Future]){
    val months = futures.map(_.delivery.asInstanceOf[Month])
    val marketDay = Day(2010, 1, 1).endOfDay
    val env = Environment(new UnitTestingAtomicEnvironment(marketDay, {
        case ForwardPriceKey(mkt, mth : Month, _) => Quantity(100 + mth.m, mkt.priceUOM)
        case DiscountRateKey(_, day, _) => new Quantity(math.exp(0.05 * day.daysSinceInYears(marketDay.day)))
        case MarketFixingKey(key, _, _) => Quantity(20, key.priceUOM)
      }
    ))
    val hedges = Hedge.futuresSpreadHedgeUTPs(env, futures, months.toSet)
    assert(hedges.exists(_.isInstanceOf[FuturesCalendarSpread]), "At least one futures spread should be created")
    val futuresComp = CompositeInstrument(futures)
    val hedgeComp = CompositeInstrument(hedges)

    for(
      key <- CollectionUtils.filterOnType[PriceDifferentiable](futuresComp.environmentDifferentiables(env.marketDay) ++ hedgeComp.environmentDifferentiables(env.marketDay))
    ){
      val delta = futuresComp.firstOrderDerivative(env, key, USD)
      val hedgeDelta = hedgeComp.firstOrderDerivative(env, key, USD)
      assertQtyEquals(delta, hedgeDelta, 1e-6)
    }
    assertQtyEquals(futuresComp.mtm(env), hedgeComp.mtm(env), 1e-6)
  }


  @Test
  def testFuturesSpreadDecompositionIsAsExpected{
    val nullEnv = Environment(new NullAtomicEnvironment(Day(2010, 4, 1).endOfDay))
    buildMonthGroups().foreach{
      months =>
        val futures = makeRandomFutures(months).filter(_.isLive(nullEnv.marketDay))
        if (!futures.isEmpty){
          val hedges = Hedge.futuresSpreadHedge(nullEnv, futures, months.toSet)
          val hedgesByType = hedges.map{
            case (h, x) => h * x
          }.groupBy(_.instrumentType)

          assert(hedgesByType.size <= 3)
          assert(hedgesByType(Future).size == 1)
          assert(hedgesByType.getOrElse(FuturesCalendarSpread, Nil).size == months.size - 1)
        }
    }
  }

  @Test
  def testSwapHedgeDeltasAreConsistent{
    val randomMonths = new RandomThing((Month(2011, 1) upto Month(2011, 12)).toList, seed = 98765)
    val randomMarkets = new RandomThing(
      List(Market.NYMEX_WTI, Market.ICE_BRENT, Market.ICE_GAS_OIL, Market.LME_LEAD),
      seed = 345)
    val u = RandomVariables.standardUniform(43434)
    val marketDay = Day(2010, 3, 13).endOfDay
    val env = Environment(new UnitTestingAtomicEnvironment(marketDay, {
        case ForwardPriceKey(mkt, mth : Month, _) => Quantity(100 + mth.m, mkt.priceUOM)
        case ForwardPriceKey(mkt, d : Day, _) => Quantity(100 + d.dayNumber, mkt.priceUOM)
        case DiscountRateKey(_, day, _) => new Quantity(math.exp(0.05 * day.daysSinceInYears(marketDay.day)))
        case IndexFixingKey(key, _) => Quantity(20, key.priceUOM)
      }
    ))
    val futures = (for (i <- 0 to 200)  yield{
      val mkt = randomMarkets.next
      val period = if (mkt.tenor == Month) randomMonths.next else randomMonths.next.firstDay + 15
      Future(
        mkt,
        period,
        Quantity(u.nextDouble * 100.0, mkt.priceUOM),
        Quantity((u.nextDouble - 0.5) * 100, mkt.uom)
      )
    }).toList
    val portfolio = CompositeInstrument(futures)

    val hedges = futures.groupBy(_.market).flatMap{
      case (_, futures) => Hedge.swapHedgeUTPs(env, futures)
    }.toList
    val hedgePortfolio = CompositeInstrument(hedges)
    for (
      key <- CollectionUtils.filterOnType[PriceDifferentiable](portfolio.atomicMarketDataKeys(env.marketDay))
    ){
      val delta = portfolio.firstOrderDerivative(env, key, USD)
      val hedgeDelta = hedgePortfolio.firstOrderDerivative(env, key, USD)
      assertQtyEquals(delta, hedgeDelta, 1e-6)
    }
    assertQtyEquals(portfolio.mtm(env), hedgePortfolio.mtm(env), 1e-6)
  }

  @Test
  def testSwapHedgesBreakOnSignShift{
    val mkt = Market.NYMEX_WTI
    val longFutures = (Month(2011, 1) upto Month(2011, 4)).toList.map{
      Future(mkt, _, Quantity(10.0, mkt.priceUOM), Quantity(20.0, mkt.uom))
    }
    val shortFutures = (Month(2011, 5) upto Month(2011, 6)).toList.map{
      Future(mkt, _, Quantity(10.0, mkt.priceUOM), Quantity(-20.0, mkt.uom))
    }
    val futures = longFutures ::: shortFutures
    val nullEnv = Environment(new NullAtomicEnvironment(Day(2010, 4, 1).endOfDay))

    val hedges = Hedge.swapHedgeUTPs(nullEnv, futures)
    val futuresMonths = CollectionUtils.filterOnType[Future](hedges).map(_.delivery).toList.distinct.sortWith(_<_)
    assertEquals(
      futuresMonths,
      List(Month(2011, 4), Month(2011, 6))
    )
  }
}
