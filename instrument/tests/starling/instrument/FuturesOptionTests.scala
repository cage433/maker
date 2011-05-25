package starling.instrument

import starling.utils.StarlingTest
import org.testng.annotations._
import org.testng.Assert._

import starling.quantity.UOM._
import starling.utils.ScalaTestUtils._
import starling.utils.QuantityTestUtils._
import starling.curves._
import starling.daterange.{Month, Day}
import starling.market.{TestExpiryRules, FuturesMarket, Market}
import starling.quantity.{UOM, Percentage, Quantity}
import starling.varcalculator.ForwardPriceRiskFactor
import starling.models._

class FuturesOptionTests extends StarlingTest {

  @DataProvider(name = "testSomeValuationsProvider")
  def testSomeValuationsProvider = {
    constructArgs(
      // test intrinsic value with near zero vol
      (365, 100.0, 102.0, 0.00001, 1.0, 0.0, 2.0),
      // Random test - used online calculator at http://www.hoadley.net/options/optiongraphs.aspx? to get results
      (5, 100.0, 100.0, 1.0, 1.0, 4.666, 4.666),
      (50, 95.0, 100.0, 0.5, 10.0, 4.961, 9.961)
    )
  }
  @Test(dataProvider = "testSomeValuationsProvider")
  def testSomeValuations(
    nDaysToExpiry : Int,
    F : Double,
    X : Double,
    volatility : Double,
    volume : Double,
    expectedCallPrice : Double,
    expectedPutPrice : Double
  ){
    val mkt = FuturesMarket.testMarket("A Test Market", USD, MT)
    val env = Environment(
      new TestingAtomicEnvironment(){
        val marketDay = Day(2009, 1, 10).endOfDay
        def applyOrMatchError(key: AtomicDatumKey) = key match {
          case _: ForwardPriceKey => Quantity(F, USD / MT)
          case _: OilAtmVolAtomicDatumKey => Percentage(volatility)
          case _: OilVolSkewAtomicDatumKey => Map[Double, Percentage]()
        }
      }
      ).undiscounted

//    val envBuilder = TestEnvironmentBuilder(marketDay)
//    envBuilder.setConstantPrice(mkt, Quantity(F, USD / MT))
//    envBuilder.setZeroRate(USD, 0.0)
//    envBuilder.setImpliedVol(mkt, volatility)
//    val env = envBuilder.build

    val exerciseDate = env.marketDay.day + nDaysToExpiry
    val futuresMaturityDate = exerciseDate
    val strike = Quantity(X, USD / MT)
    val callAndPutOptions = List(Call, Put).map(
      new FuturesOption(
      	mkt,
      	exerciseDate,
      	futuresMaturityDate,
      	Quantity(X, USD / MT),
      	Quantity(volume, MT),
      	_,
        European)
    )

    val List(callMtm, putMtm) = callAndPutOptions.map(_.mtm(env))
    assertQtyEquals(callMtm, Quantity(volume, MT) * Quantity(expectedCallPrice, USD / MT), volume * 1e-3)
    assertQtyEquals(putMtm, Quantity(volume, MT) * Quantity(expectedPutPrice, USD / MT), volume * 1e-3)
  }

  @Test
  def testTheta() {
    val env = Environment(
      new TestingAtomicEnvironment(){
        val marketDay = Day(2009, 1, 10).endOfDay
        def applyOrMatchError(key: AtomicDatumKey) = key match {
          case _: ForwardPriceKey => Quantity(101, USD / MT)
          case _: OilAtmVolAtomicDatumKey => Percentage(0.20)
          case _: OilVolSkewAtomicDatumKey => Map[Double, Percentage]()
        }
      }
    ).undiscounted

    val exerciseDay = env.marketDay.day + 100
    val option = new FuturesOption(
      	FuturesMarket.testMarket("A Test Market", USD, MT, Month),
      	exerciseDay,
      	Month(2012, 10),
      	Quantity(100, USD / MT),
      	Quantity(100, MT),
      	Call,
        European)

    val timeValue = option.mtm(env, UOM.USD) - option.mtm(env.zeroVols, USD)
    val thetaToExerciseDay = option.theta(env, exerciseDay.endOfDay, USD)
    assertEquals(timeValue * -1, thetaToExerciseDay)

    val thetaAfterExerciseDay = option.theta(env, (exerciseDay + 20).startOfDay, USD)
    assertEquals(thetaToExerciseDay, thetaAfterExerciseDay)
  }


  @DataProvider(name = "testNumericDelta")
  def testNumericDeltaData = {
    constructArgs(
      (Market.ICE_BRENT, Quantity(118.7, USD / BBL)),
      (Market.ICE_GAS_OIL, Quantity(997, USD / MT)),
      (Market.ICE_WTI, Quantity(107.94, USD / BBL)),
      (Market.NYMEX_GASOLINE, Quantity(3.1345, USD / GAL)),
      (Market.NYMEX_GASOLINE, Quantity(313.45, US_CENT / GAL)),
      (Market.NYMEX_HEATING, Quantity(3.1513, USD / GAL))
    )
  }

  @Test(dataProvider = "testNumericDelta")
  def testNumericDelta(market: FuturesMarket, price: Quantity) {
    val vol = new Percentage(.4)
    val md = Day(2011, 1, 1)
    val env = Environment(
      new UnitTestingAtomicEnvironment(md.endOfDay, {
        case ForwardPriceKey(`market`, _, _) => price
        case _: OilAtmVolAtomicDatumKey => vol
        case _: OilVolSkewAtomicDatumKey => Map[Double, Percentage]()
      })
    ).undiscounted

    val month = Month(2011, 5)
    val expiry = market.optionExpiry(month)
    val T = expiry.endOfDay.timeSince(md.endOfDay)

    List(Call, Put).map {
      cp => {
        val option = new FuturesOption(market, expiry, month, price, new Quantity(1, market.uom), cp, European)

        val bs = new BlackScholes(price.value, price.value, cp, T, vol.decimalValue)

        assertQtyEquals(option.delta(env, USD).value, bs.analyticDelta, 1e-5)
      }
    }
  }

}
