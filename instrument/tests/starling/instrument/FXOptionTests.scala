package starling.instrument

import starling.utils.StarlingTest
import starling.quantity.UOM._
import starling.curves._
import starling.daterange.{TimeOfDay, DayAndTime, Day}
import org.testng.annotations.{DataProvider, Test}
import starling.quantity.{UOM, Percentage, Quantity}
import org.testng.Assert._
import starling.quantity.utils.QuantityTestUtils._
import starling.models.{European, Put, Call}
import starling.market.{Market, FuturesMarket, FXMarket}

class FXOptionTests extends StarlingTest {

  val marketDay = Day(2010, 1, 1)
  def environment(timeOfDay : TimeOfDay) : Environment = {

    val atomicEnv = new TestingAtomicEnvironment{
      def marketDay = DayAndTime(FXOptionTests.this.marketDay, timeOfDay)

      def discountRate(ccy : UOM, day : Day) = {
        val z = ccy match {
          case `USD` => 0.05
          case `GBP` => 0.03
        }
        val t = day.daysSinceInYears(marketDay.day)
        new Quantity(math.exp(- z * t))
      }
      def applyOrMatchError(key : AtomicDatumKey) = key match {
        case USDFXRateKey(`GBP`) => Quantity(1.05, USD/GBP)
        case _ : ForwardPriceKey => Quantity(1.05, USD/MT)
        case _ : OilAtmVolAtomicDatumKey => Percentage(0.3)
        case _ : OilVolSkewAtomicDatumKey => Map[Double, Percentage]()
        case BradyFXVolSmileAtomicDatumKey(FXMarket(USD, GBP), _) => Map(0.5 -> Percentage(0.3))
        case DiscountRateKey(ccy, forwardDate, _) => discountRate(ccy, forwardDate)
      }
    }

    Environment(atomicEnv)
  }

  /**
   * Supply lots of options with various strikes, volumes etc
   */
  @DataProvider(name = "fxOptionsProvider")
  def fxOptionsProvider = {
    val exerciseDay: Day = Day(2012, 9, 30)
    val maturityDay: Day = exerciseDay

    val parameterList = for (
      strikeUOM <- List(USD/GBP, GBP/USD);
      volumeCCY <- List(USD, GBP);
      strikeValue <- List(0.93, 0.98, 1.04, 1.1);
      callOrPut <- List(Call, Put)
    )
      yield Array[Object](FXOption(Quantity(strikeValue, strikeUOM), Quantity(100.0, volumeCCY), exerciseDay, maturityDay, callOrPut))

    parameterList.toArray
  }

  def correspondingForward (option : FXOption) : FXForward = {
    FXForward(option.strike, option.volume * (if (option.callPut == Call) 1.0 else -1.0), option.maturityDate)
  }

  @Test(dataProvider = "fxOptionsProvider")
  def mtmShouldMatchIntrinsicWhenVolatilityIsZero(option : FXOption)
  {

    val env = environment(TimeOfDay.StartOfDay).zeroVols

    val forward = correspondingForward(option)

    assertEquals(option.mtm(env).uom, option.valuationCCY)
    if (option.isInTheMoney(env)){
      assertQtyEquals(option.mtm(env), forward.asUtpPortfolio.mtm(env, option.valuationCCY), 1e-6)
    } else {
      assertQtyEquals(option.mtm(env), Quantity(0, option.valuationCCY), 1e-6)
    }
  }

  @Test(dataProvider = "fxOptionsProvider")
  def mtmShouldStaySameWhenStrikeIsInverted (option : FXOption){
    val invertedOption = FXOption(option.strike.invert, option.volume, option.exerciseDate, option.maturityDate, option.callPut)
    val env = environment(TimeOfDay.StartOfDay)
    assertQtyEquals(option.mtm(env), invertedOption.mtm(env), 1e-6)
  }

  @Test
  def expiredOptionShouldHaveZeroMTM{
    val env = environment(TimeOfDay.EndOfDay)

    val exerciseDay: Day = marketDay
    val maturityDay: Day = exerciseDay + 1

    val option = FXOption(Quantity(1.0, USD/GBP), Quantity(100.0, GBP), exerciseDay, maturityDay, Call)

    assertQtyEquals(option.mtm(env), Quantity(0.0, USD), 1e-9)
  }

  @Test
  def optionShouldHaveSameValueAsAFuturesOption{
    val env = environment(TimeOfDay.StartOfDay).undiscounted

    val exerciseDay: Day = marketDay + 100
    val maturityDay: Day = exerciseDay + 10

    for (
      callOrPut <- List(Call, Put);
      strike <- List(1.3, 1.5, 1.7)
    ) {
      val option = FXOption(Quantity(strike, USD/GBP), Quantity(100.0, GBP), exerciseDay, maturityDay, callOrPut)
      val futuresOption = new FuturesOption(Market.testMarket("LME", USD, MT), exerciseDay, maturityDay, Quantity(strike, USD/MT), Quantity(100, MT), callOrPut, European)

      assertQtyEquals(option.mtm(env), futuresOption.mtm(env), 1e-6)
    }
  }

  @Test
  def testExplanation{
    val exerciseDay: Day = marketDay + 100
    val maturityDay: Day = exerciseDay + 10
    val env = environment(TimeOfDay.StartOfDay)
    val option = FXOption(Quantity(1.1, USD/GBP), Quantity(100.0, GBP), exerciseDay, maturityDay, Call)
    val explanation = option.explanation(env)
    assertEquals(explanation.name, "((BlackScholes-Call(F, K, Vol, T) * Volume) * Discount)")
    assertEquals(explanation.format(1), "((BlackScholes-Call(((USD per GBP spot * GBP.21Apr2010) / USD.21Apr2010), 1.10 USD per GBP, 30.00%, 0.28) * 100.00 GBP) * USD.11Apr2010)")
    val lastExplanation = "((BlackScholes-Call(((1.05 USD per GBP * 0.99) / 0.99), 1.10 USD per GBP, 0.30, 0.28) * 100.00 GBP) * 0.99)"
    assertEquals(explanation.format(2), lastExplanation)
    assertEquals(explanation.format(3), lastExplanation)
  }


}
