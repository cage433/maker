package starling.instrument


import starling.utils.StarlingTest
import starling.curves._
import starling.maths.{LognormalCalcs, RandomVariables}
import starling.quantity.{Percentage, Quantity}
import cern.colt.matrix.impl.{DenseDoubleMatrix1D => DVector}
import cern.colt.matrix.{DoubleMatrix1D => Vector}
import starling.daterange.TimeOfDay._
import org.testng.annotations.{DataProvider, Test}
import org.testng.Assert._
import starling.quantity.UOM._
import starling.utils.ImplicitConversions._
import starling.utils.conversions.RichColtMatrices._
import starling.utils.ScalaTestUtils._
import java.lang.Math._
import java.lang.Math
import starling.utils.QuantityTestUtils._
import starling.quantity.Percentage._
import starling.models.{European, CallOrPut, Put, Call}
import starling.utils.{Log, CollectionUtils, ScalaTestUtils}
import starling.market._
import starling.varcalculator.ForwardPriceRiskFactor
import starling.daterange._

class AsianOptionTests extends JonTestEnv {

  @DataProvider(name ="testLognormalAverageProvider")
  def testLognormalAverageProvider = {
    constructArgs(
      (List(1.0), List(0.5), 0.5)
      ,(List(0.5), List(0.5), 0.5)
      ,(List(0.5), List(0.0), 0.5)
      ,(List(1.0, 1.0), List(0.5, 1.0), 0.5)
      ,(List(3.0, 3.0), List(0.5, 1.0), 0.5)
      ,(List(1.0, 3.0), List(0.5, 0.50001), 0.5)
      ,(List(1.0, 3.0), List(0.5, 1.0), 0.5)
      ,(List(1.0, 3.0, 2.0), List(0.5, 1.0, 2.0), 0.5)
      )
  }

  @Test(dataProvider = "testLognormalAverageProvider")
  /** Run monte carlo simulations to calculate the average observed price from the
   *  evolutions of a given forward curve. Check that the variance of this average
   *  matches the analytic value. 
   */
  def testLognormalAverage(prices: List[Double], times: List[Double], sigma: Double) {
    val sn = RandomVariables.standardNormal(12345)
    val nPrices = prices.size
    def brownianMove(dt: Double): Double = sn.nextDouble * Math.sqrt(dt)
    val dts: List[Double] = {
      val timeList = times.toArray.toList
      ((0.0 :: timeList).zip(timeList)).map {case (t1, t2) => t2 - t1}
    }
    val riskAdjustments = new DVector(times.toArray) * (-0.5 * sigma * sigma)
    def makeBrownian: Vector = {
      var b = 0.0
      val brownian = new DVector(nPrices)
      dts.zipWithIndex.foreach {
        case (dt, i) =>
          b += sn.nextDouble * Math.sqrt(dt)
          brownian(i) = b
      }
      brownian
    }
    def makePath: Vector = {
      (makeBrownian * sigma + riskAdjustments).exp |*| new DVector(prices.toArray)
    }
    def averagePayoff = makePath.zSum / nPrices
    val nSamples = 100000
    val sample = {
      val s = new DVector(nSamples)
      for (i <- 0 until nSamples) {
        s(i) = averagePayoff
      }
      s
    }
    val observedVariance = sample.variance
    val expectedVariance = LognormalCalcs.varianceOfAverage(prices.toArray.toList, times.toArray.toList, sigma)
    assertEquals(observedVariance, expectedVariance, 0.02)
  }

  @Test
  /** In the absence of vols, and if forward prices become fixings, then an asian option should keep
   * the same value during its life
   */
  def testAsianPriceDuringLife {
    val sep09 = Month(2009, 9)
    val su = RandomVariables.standardUniform(12345)
    val index = Index.PREM_UNL_EURO_BOB_OXY_NWE_BARGES
    val market = index.market
    val averagingDays = index.observationDays(sep09)
    val prices = Map[DateRange, Quantity]() ++ averagingDays.map{d => index.observedPeriod(d) -> Quantity(100.0 * su.nextDouble, USD/ MT)}
    val fixings = Map[DateRange, Quantity]() ++ averagingDays.map{d => d -> prices(index.observedPeriod(d))}
    def fixingDays(marketDay : DayAndTime) = averagingDays.filter(_.endOfDay <= marketDay)


    def buildEnvironment(index: Index, envMarketDay: DayAndTime, vol : Double): Environment = {
      Environment(
        new TestingAtomicEnvironment() {
          def marketDay = envMarketDay

          def applyOrMatchError(key: AtomicDatumKey) = key match {
            case _ : OilAtmVolAtomicDatumKey => Percentage(vol)
            case _: OilVolSkewAtomicDatumKey => Map[Double, Percentage]()
            case _: DiscountRateKey => new Quantity(1.0)
            case _: BradyMetalVolAtomicDatumKey => new Percentage(vol)
            case ForwardPriceKey(_, period, _) => prices(period)
            case FixingKey(_, day) => fixings(day)
          }
        }
        )
    }
    val originalEnv = buildEnvironment(index, Day(2009, 1, 1).startOfDay, 0)
    for (
      day <- Day(2009, 8, 1) :: averagingDays;
      timeOfDay <- List(StartOfDay, EndOfDay);
      callPut <- List(Call, Put);
      vol <- List(0, 1e-6);
      if day < averagingDays.last || timeOfDay == StartOfDay
    ) {
      val strike = if (callPut == Call) Quantity(20, USD / MT) else Quantity(80, USD / MT)
      val marketDay = DayAndTime(day, timeOfDay)
      val option = SingleAsianOption(index, sep09, strike, Quantity(13, MT), callPut)
      val originalMtm = option.mtm(originalEnv)
      val env = buildEnvironment(index, marketDay, vol)
      val mtmAtThisTime = option.mtm(env)
      assertQtyEquals(mtmAtThisTime, originalMtm, 1e-6)
      // Just check that this test hasn't randomly produced prices that make the options worthless
      assertTrue(mtmAtThisTime.value > 1.0)
    }
  }

  @DataProvider(name = "testLongDatedOptionProvider")
  def testLongDatedOptionProvider = {
    constructArgs(
      (Day(2015, 1, 31), 100.0, 100.0, 0.5, Call)
      , (Day(2015, 1, 31), 110.0, 95.0, 0.3, Put)
      , (Day(2012, 1, 31), 110.0, 95.0, 0.3, Put)
      )
  }

  @Test(dataProvider = "testLongDatedOptionProvider")
  def testLongDatedAsianIsCloseToVanillaValue(lastAveragingDay: Day, price: Double, strike: Double, volatility: Double, callPut: CallOrPut) {
    val marketDay = Day(2009, 1, 1)
    val period = (lastAveragingDay - 30) upto lastAveragingDay
    val env = Environment(
      new TestingAtomicEnvironment() {
        def marketDay = Day(2009, 1, 1).endOfDay

        def applyOrMatchError(key: AtomicDatumKey) = key match {
          case DiscountRateKey(_, day, _) => new Quantity(Math.exp(-0.05 * day.daysSinceInYears(marketDay.day)))
          case _ : OilAtmVolAtomicDatumKey => Percentage(volatility)
          case _: OilVolSkewAtomicDatumKey => Map[Double, Percentage]()

          case _: BradyMetalVolAtomicDatumKey => Percentage(volatility)
          case _: ForwardPriceKey => Quantity(price, USD / MT)
          case _: FixingKey => Quantity(price, USD / MT)
        }
      }
      )

    val index = Index.GO11
    val market = index.market

    val strikeQty = Quantity(strike, USD / MT)
    val volume = Quantity(123, MT)
    val asianOption = SingleAsianOption(index, period, strikeQty, volume, callPut)
    val middleDay = period.firstDay + period.days.size / 2
    val vanillaOption = new FuturesOption(market, period.firstDay - 1, period.firstDay.containingMonth, strikeQty, volume, callPut, European)

    val asianMTM = asianOption.mtm(env)
    val vanillaMTM = vanillaOption.mtm(env)
    assertQtyEquals(asianMTM, vanillaMTM, asianMTM.value * 0.02)
  }

  @Test
  def fieldsOnTradeableTypeTest {
    val actualFields = AsianOption.fields
    assertEquals(actualFields, List("Market", "Period", "Quantity", "Strike", "Call Put"))
  }

  @Test
  def testFreightEquivalentAsian {
    val period = Month(2009, 10)
    val middleDay = period.firstDay + period.days.size / 2
    val freightIndex = Index.CAPSIZE_TC_AVG
    val index = Index.PREM_UNL_EURO_BOB_OXY_NWE_BARGES
    val market = index.market
    val md = Day(2009, 9, 10)

    val averagingDays: List[Day] = market.observationDays(period)
    val fixedDays = averagingDays.filter(_.endOfDay <= md.endOfDay)
    val unfixedDays = averagingDays.filter(_.endOfDay > md.endOfDay)
    val nFixings = fixedDays.size
    val nUnfixed = unfixedDays.size

    val env = Environment(
      new TestingAtomicEnvironment() {
        def marketDay = md.endOfDay

        def applyOrMatchError(key: AtomicDatumKey) = key match {
          case DiscountRateKey(_, day, _) => new Quantity(1.0)
          case _ : OilAtmVolAtomicDatumKey => Percentage(0.8)
          case _: OilVolSkewAtomicDatumKey => Map[Double, Percentage]()
          case _: BradyMetalVolAtomicDatumKey => Percentage(.8)
          case ForwardPriceKey(`freightIndex`, _, _) => Quantity(65, freightIndex.priceUOM)
          case ForwardPriceKey(market, _, _) => {
            val n = nFixings / (nFixings + nUnfixed * 1.0)
            val m = nUnfixed / (nFixings + nUnfixed * 1.0)
            val fixedAverage = 50.0
            val forwardPrice = fixedAverage * n + 65.0 * m
            Quantity(forwardPrice, market.priceUOM)
          }
          case FixingKey(`freightIndex`, _) => Quantity(50, freightIndex.priceUOM)
          case FixingKey(`index`, _) => Quantity(50, market.priceUOM)
        }
      }
      )

    val metalsAO = SingleAsianOption(index, period, Quantity(60, market.priceUOM), Quantity(1, market.uom), Call)
    val freightAO = SingleAsianOption(freightIndex, period, Quantity(60, freightIndex.priceUOM), Quantity(1, freightIndex.uom), Call)

    val mtm1 = metalsAO.mtm(env)
    val mtm2 = freightAO.mtm(env)
    assertQtyEquals(mtm1, mtm2)
  }


  @Test
  def testAsianStrip {
    val env = makeEnv(Day(2010, 1, 1).endOfDay)
    val index = Index.WTI10
    val jan = Month(2010, 1)
    val feb = Month(2010, 2)
    val mar = Month(2010, 3)
    val period = Strip(jan, mar)
    val strike = Quantity(50, USD/BBL)
    val volume = Quantity(1000, BBL)
    val option1 = SingleAsianOption(index, jan, strike, volume, Call)
    val option2 = SingleAsianOption(index, feb, strike, volume, Call)
    val option3 = SingleAsianOption(index, mar, strike, volume, Call)

    val mtm1 = option1.mtm(env)
    val mtm2 = option2.mtm(env)
    val mtm3 = option3.mtm(env)

    val asian = AsianOption(index, period, strike, volume, Call)

    assertEquals(asian.asUtpPortfolio(Day(2009, 1, 1)).portfolio, List(option1, option2, option3).map(o => (o.copy(volume = Quantity(1, BBL)) -> o.volume.value)).toMap)
    assertQtyEquals(asian.asUtpPortfolio(Day(2009, 1, 1)).mtm(env), mtm1 + mtm2 + mtm3)
  }

  @Test
  def testTheFuturesMonthVegaWhenATMVegaIsUncheckedIsTheSameAsScalingTheSwapVegaByTheObservationDays {
    val env = Environment(
      new UnitTestingAtomicEnvironment(DayAndTime(Day(2010, 1, 1), TimeOfDay.EndOfDay), {
        key => key match {
          case _ : DiscountRateKey => new Quantity(1.0)
          case ForwardPriceKey(market, _, _) => Quantity(100.0, market.priceUOM)
          case _ : OilAtmVolAtomicDatumKey => Percentage(0.10)
          case _ : OilVolSkewAtomicDatumKey => Map(0.5 -> Percentage(0.01), 0.1 -> Percentage(0.02), 0.9 -> Percentage(0.03))
        }
      })
    )

    val nonSkewEnv = env.setShiftsCanBeIgnored(true)

    val feb = Month(2012, 2)
    val march = feb + 1
    val april = feb + 2

    val index = Index.WTI10
    val option = SingleAsianOption(
      index,
      feb,
      Quantity(105.0, index.priceUOM),
      Quantity(100, index.uom),
      Put
    )

    // test swap vol is sum of futures vols 

    for (interp_vols <- List(true, false)){
    
      val marchVega = option.vega(env, OilAtmVolAtomicDatumKey(Market.NYMEX_WTI, None, march), shiftInterpolatedVols = interp_vols)
      val aprilVega = option.vega(env, OilAtmVolAtomicDatumKey(Market.NYMEX_WTI, None, april), shiftInterpolatedVols = interp_vols)
      val swapVega = if(interp_vols)
        option.vega(env, SwapVol(index, feb), shiftInterpolatedVols = interp_vols)
      else {
        index.observationDays(feb).map{
          d => 
          val vega = option.vega(env, OilAtmVolAtomicDatumKey(Market.NYMEX_WTI, Some(d), index.observedOptionPeriod(d)))
          vega
        }.sum
      }
      val parallelVega = option.parallelVega(env, interp_vols)
      assertQtyEquals(aprilVega + marchVega, swapVega, swapVega.value * 1e-4)
      assertQtyEquals(swapVega, parallelVega, parallelVega.value * 1e-4);
  
    }
  }
}
