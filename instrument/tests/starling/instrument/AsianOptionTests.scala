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
import starling.quantity.utils.QuantityTestUtils._
import starling.quantity.Percentage._
import starling.models.{European, CallOrPut, Put, Call}
import starling.utils.{Log, CollectionUtils, ScalaTestUtils}
import starling.market._
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
            case ForwardPriceKey(_, period, _) => prices(period)
            case IndexFixingKey(_, day) => fixings(day)
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

          case _: ForwardPriceKey => Quantity(price, USD / MT)
          case _: IndexFixingKey => Quantity(price, USD / MT)
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
  def testExplanation() {
    val marketDay = Day(2009, 1, 1)
    val period = Month(2010, 1)
    val fwdPrice = Quantity(101, USD/MT)
    val fixingPrice = Quantity(99, USD/MT)
    val env = Environment(
      new TestingAtomicEnvironment() {
        def marketDay = Day(2009, 1, 1).endOfDay

        def applyOrMatchError(key: AtomicDatumKey) = key match {
          case DiscountRateKey(_, day, _) => new Quantity(Math.exp(-0.05 * day.daysSinceInYears(marketDay.day)))
          case _ : OilAtmVolAtomicDatumKey => Percentage(0.2)
          case _: OilVolSkewAtomicDatumKey => Map[Double, Percentage]()

          case _: ForwardPriceKey => fwdPrice
          case _: IndexFixingKey => fixingPrice
        }
      }
      )

    val index = Index.GO11
    val market = index.market

    val strikeQty = Quantity(100, USD / MT)
    val volume = Quantity(123, MT)
    val asianOption = SingleAsianOption(index, period, strikeQty, volume, Call)
    val explanation = asianOption.explanation(env)
    assertEquals(explanation.name, "((Curran-Call(Average(IPE Gas Oil 1st month (Settlement).JANUARY 2010), Vol, K) × Volume) × Discount)")

    val ex0 = "((Curran-Call(Average(01Jan2010, 04Jan2010, 05Jan2010, 06Jan2010, 07Jan2010, 08Jan2010, 11Jan2010, " +
            "12Jan2010, 13Jan2010, 14Jan2010, 15Jan2010, 18Jan2010, 19Jan2010, 20Jan2010, 21Jan2010, 22Jan2010, " +
            "25Jan2010, 26Jan2010, 27Jan2010, 28Jan2010, 29Jan2010), 20.00%, 100.00 USD/MT) × 123.00 MT) × USD.29Jan2010)"

    assertEquals(explanation.format(1), ex0)

    val ex = "((Curran-Call(Average(JAN 2010, JAN 2010, JAN 2010, " +
            "JAN 2010, JAN 2010, JAN 2010, JAN 2010, " +
            "FEB 2010, FEB 2010, FEB 2010, FEB 2010, " +
            "FEB 2010, FEB 2010, FEB 2010, FEB 2010, " +
            "FEB 2010, FEB 2010, FEB 2010, FEB 2010, " +
            "FEB 2010, FEB 2010), 0.20, 100.00 USD/MT) × 123.00 MT) × 0.95)"

    val ex1 = "((Curran-Call(Average(101.00 USD/MT, 101.00 USD/MT, 101.00 USD/MT, 101.00 USD/MT, 101.00 USD/MT," +
            " 101.00 USD/MT, 101.00 USD/MT, 101.00 USD/MT, 101.00 USD/MT, 101.00 USD/MT, 101.00 USD/MT, 101.00 USD/MT, " +
            "101.00 USD/MT, 101.00 USD/MT, 101.00 USD/MT, 101.00 USD/MT, 101.00 USD/MT, 101.00 USD/MT, 101.00 USD/MT, " +
            "101.00 USD/MT, 101.00 USD/MT), 0.20, 100.00 USD/MT) × 123.00 MT) × 0.95)"

    assertEquals(explanation.format(2), ex)
    assertEquals(explanation.format(3), ex1)
    assertEquals(explanation.format(4), ex1)
  }

  @Test
  def fieldsOnTradeableTypeTest {
    val actualFields = AsianOption.fields
    assertEquals(actualFields, List("Market", "Period", "Quantity", "Strike", "Call Put"))
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
    val env = UnitTestingEnvironment(DayAndTime(Day(2010, 1, 1), TimeOfDay.EndOfDay), {
      key => key match {
        case _ : DiscountRateKey => new Quantity(1.0)
        case ForwardPriceKey(market, _, _) => Quantity(100.0, market.priceUOM)
        case _ : OilAtmVolAtomicDatumKey => Percentage(0.10)
        case _ : OilVolSkewAtomicDatumKey => Map(0.5 -> Percentage(0.01), 0.1 -> Percentage(0.02), 0.9 -> Percentage(0.03))
      }
    })

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
