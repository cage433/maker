package starling.instrument

import starling.utils.StarlingTest
import cern.colt.matrix.impl.{DenseDoubleMatrix1D => DVector, DenseDoubleMatrix2D => DMatrix}
import starling.maths.{AcklamInverseNormal, MatrixUtils}
import org.testng.annotations._
import org.testng.Assert._
import org.mockito.Mockito._
import org.mockito.Matchers._
import org.mockito.Matchers.eq
import starling.quantity.UOM._
import starling.utils.ScalaTestUtils._
import starling.varcalculator._
import starling.quantity.{UOM, Quantity}
import org.mockito.{ArgumentMatcher, ArgumentCaptor}
import reflect.Manifest
import starling.market.SpotUSDFXRiskFactorType
import starling.curves._
import starling.daterange.{TimeOfDay, DayAndTime, Day}
import starling.utils.QuantityTestUtils._

class FXForwardTests extends StarlingTest {

  @DataProvider(name = "deltaPosition_UOM_provider")
  def deltaPosition_UOM_provider : Array[Array[Object]] = {
    val currencies = List(USD, EUR, GBP)
    val arguments = for (
      numeratorCCY <- currencies;
      denominatorCCY <- currencies.filterNot(_ == numeratorCCY);
      posisitonCCY <- currencies.filterNot(_ == USD)
      )
        yield Array[Object](numeratorCCY / denominatorCCY, posisitonCCY)
      arguments.toArray
  }
  @Test(dataProvider = "deltaPosition_UOM_provider")
  def deltaPosition_UOMS_should_always_be_in_the_non_usd_currency(fxUnit : UOM, positionCCY : UOM){
    val spotFxRates = Map(GBP -> 2.0, EUR -> 1.5, CHF -> 1.4)
    val atomicEnv  = new TestingAtomicEnvironment{
      def marketDay = Day(2008, 1, 1).endOfDay

      def applyOrMatchError(key: AtomicDatumKey) = key match {
        case USDFXRateKey(ccy) => Quantity(spotFxRates(ccy), USD/ccy)
        case _ : DiscountRateKey => new Quantity(1.0)
      }
    }
    val env = Environment(atomicEnv)
    val fxForward = FXForward(Quantity(1.3, fxUnit), Quantity(1000, fxUnit.denominatorUOM), Day(2009, 1, 1))
    val position = fxForward.asUtpPortfolio.riskFactorPosition(env, SpotFXRiskFactor(positionCCY), positionCCY)
    assertEquals(position.uom, positionCCY)
  }

	@Test
	/** Value a typical FX Forward and check its mtm is as expected
  */
	def testMTM{
	  val marketDay = Day(2009, 10, 1).endOfDay
	  val envBuilder = TestEnvironmentBuilder(marketDay)
    val usdPerGbp = Quantity(0.8, USD / GBP)
    val usdPerEur = Quantity(1.1, USD / EUR)

	  envBuilder.setUSDPerCCYSpotRate(GBP, usdPerGbp)
	  envBuilder.setUSDPerCCYSpotRate(EUR, usdPerEur)
	  val usdZeroRate = 5.5
	  val gbpZeroRate = 0.2
	  val eurZeroRate = 0.1
	  envBuilder.setZeroRate(USD, usdZeroRate)  // Note that this rate is irrelevant to this instruments value
	  envBuilder.setZeroRate(GBP, gbpZeroRate)
	  envBuilder.setZeroRate(EUR, eurZeroRate)
	  val env = envBuilder.build

	  val volume = Quantity(1000.0, GBP)
	  val strike = Quantity(1.4, EUR / GBP)
	  val maturityDate = Day(2010, 2, 10)
	  val forwardFX = FXForward(strike, volume, maturityDate)
	  val forwardEurPerGbp = env.forwardFXRate(EUR, GBP, maturityDate)
	  
	  assertQtyEquals(
     volume * (forwardEurPerGbp - strike) * env.discount(EUR, maturityDate),
     forwardFX.mtm(env),
     1e-6)
   
     // check its value is the same as the corresponding cash instruments
    val gbpPayment = new CashInstrument(volume, maturityDate)
    val eurPayment = new CashInstrument(volume * strike, maturityDate)
    assertQtyEquals(
      gbpPayment.mtm(env, EUR) - eurPayment.mtm(env),
      forwardFX.mtm(env),
      1e-6
    )
	}

  @Test
  /** Value an FX Forward with inverted strike units
  */
  def testMTMWithInvertedStrike{
    val marketDay = Day(2009, 10, 1).endOfDay
    val envBuilder = TestEnvironmentBuilder(marketDay)
    val usdPerGbp = Quantity(0.8, USD / GBP)
    val usdPerEur = Quantity(1.1, USD / EUR)

    envBuilder.setUSDPerCCYSpotRate(GBP, usdPerGbp)
    envBuilder.setUSDPerCCYSpotRate(EUR, usdPerEur)
    val usdZeroRate = 5.5
    val gbpZeroRate = 0.2
    val eurZeroRate = 0.1
    envBuilder.setZeroRate(USD, usdZeroRate)  // Note that this rate is irrelevant to this instruments value
    envBuilder.setZeroRate(GBP, gbpZeroRate)
    envBuilder.setZeroRate(EUR, eurZeroRate)
    val env = envBuilder.build

    val volume = Quantity(1000.0, GBP)
    val strike = Quantity(1.4, EUR / GBP).invert
    val maturityDate = Day(2010, 2, 10)
    val forwardFX = FXForward(strike, volume, maturityDate)
    val forwardEurPerGbp = env.forwardFXRate(EUR, GBP, maturityDate)

    assertQtyEquals(
     volume * (forwardEurPerGbp - strike.invert) * env.discount(EUR, maturityDate),
     forwardFX.mtm(env),
     1e-6)

     // check its value is the same as the corresponding cash instruments
    val gbpPayment = new CashInstrument(volume, maturityDate)
    val eurPayment = new CashInstrument(volume * strike.invert, maturityDate)
    assertQtyEquals(
      gbpPayment.mtm(env, EUR) - eurPayment.mtm(env),
      forwardFX.mtm(env),
      1e-6
    )
  }

  @Test
  def testExplanation{
    val marketDay = Day(2011, 8, 2).endOfDay
    val spotRates = Map(GBP -> Quantity(1.5, USD/GBP), EUR -> Quantity(1.2, USD/EUR))
    val zeroRates = Map(GBP -> 0.05, EUR -> 0.02, USD -> 0.1)
    val env = Environment(
      new UnitTestingAtomicEnvironment(
        marketDay,
        {
          case USDFXRateKey(ccy) => spotRates(ccy)
          case DiscountRateKey(ccy, day, _) => new Quantity(math.exp(zeroRates(ccy)) * day.endOfDay.timeSince(marketDay))
        }
      )
    )
    val forwardFX = FXForward(Quantity(1.25, EUR/GBP), Quantity(1000, GBP), Day(2012, 12, 1))
    val explanation = forwardFX.explanation(env)
    assertEquals(explanation.name, "(((Rec * ((((1/USD per EUR spot) * USD per GBP spot) * GBP.01Dec2012) / EUR.01Dec2012)) + Pay) * Discount)")
    assertEquals(explanation.format(1), "(((1,000.00 GBP * ((((1/1.20 USD per EUR) * 1.50 USD per GBP) * 1.40) / 1.36)) + (1,250.00) EUR) * EUR.01Dec2012)")
    val lastExplanation = "(((1,000.00 GBP * ((((1/1.20 USD per EUR) * 1.50 USD per GBP) * 1.40) / 1.36)) + (1,250.00) EUR) * 1.36)"
    assertEquals(explanation.format(2), lastExplanation)
    assertEquals(explanation.format(3), lastExplanation)
  }
}
