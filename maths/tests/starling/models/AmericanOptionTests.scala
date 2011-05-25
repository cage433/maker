package starling.models

import org.scalatest.testng.TestNGSuite
import org.testng.annotations.Test
import starling.utils.QuantityTestUtils._
import starling.quantity.Quantity

/**
 * These were Jon's tests for his american option solver
 */
class AmericanOptionTests extends TestNGSuite {

  private def valueAmericanOption(callPut : CallOrPut, F : Double, K : Double, vol : Double, r : Double, T : Double) = {
    val timeStepBuilder = new FixedTimesBuilder(100)
    val fdParams = DefaultValuationParameters.copy(timeStepBuilder = timeStepBuilder, width = 200)
    val times = timeStepBuilder.times(T)
    new CrankNicholsonOptionPricer(
      fdParams,
      times,
      F,
      vol,
      r, callPut, K
      ).valueAmericanWithCorrection()
  }
  @Test
  def AtTheMoneyCNCallTest = {
		val cn = valueAmericanOption(Call, 100, 100, 0.2, 0.05, 1)
		assertQtyEquals(cn, 7.662, 1e-3)
  }
  
  @Test
	def AtTheMoneyCNPutTest = {
		val cn = valueAmericanOption(Put, 100, 100, 0.2, 0.05, 1)
		assertQtyEquals(cn,  7.662, 1e-3)
	}

	@Test
	def InTheMoneyCNCallTest = {
		val cn = valueAmericanOption(Call, 150, 100, 0.2, 0.05, 1)
		assertQtyEquals(cn, 49.9999, 1e-4)
	}

	@Test
	def OutOfTheMoneyCNPutTest = {
		val cn = valueAmericanOption(Put, 150, 100, 0.2, 0.05, 1)
		assertQtyEquals(cn,  0.1836, 1e-4)
	}

	@Test
	def OutOfTheMoneyCNCallTest = {
    val cn = valueAmericanOption(Call, 50, 100, 0.2, 0.05, 1)
		assertQtyEquals(cn, 0.000897, 1e-4)
	}

	@Test
	def InTheMoneyCNPutTest = {
    val cn = valueAmericanOption(Put, 50, 100, 0.2, 0.05, 1)
		assertQtyEquals(cn, 49.9999, 1e-3)
	}



  @Test
  def AtTheMoneyCNCallTest2 = {
		val cn = valueAmericanOption(Call, 100, 100, 0.2, 0.05, 1)
		assertQtyEquals(cn, 7.662, 1e-3)
  }
  
  @Test
	def AtTheMoneyCNPutTest2 = {
		val cn = valueAmericanOption(Put, 100, 100, 0.2, 0.05, 1)
		assertQtyEquals(cn,  7.662, 1e-3)
	}

	@Test
	def InTheMoneyCNCallTest2 = {
		val cn = valueAmericanOption(Call, 150, 100, 0.2, 0.05, 1)
		assertQtyEquals(cn, 49.9999, 1e-3)
	}

	@Test
	def OutOfTheMoneyCNPutTest2 = {
		val cn = valueAmericanOption(Put, 150, 100, 0.2, 0.05, 1)
		assertQtyEquals(cn,  0.1836, 1e-3)
	}

	@Test
	def OutOfTheMoneyCNCallTest2 = {
    val cn = valueAmericanOption(Call, 50, 100, 0.2, 0.05, 1)
		assertQtyEquals(cn, 0.000897, 1e-3)
	}

	@Test
	def InTheMoneyCNPutTest2 = {
    val cn = valueAmericanOption(Put, 50, 100, 0.2, 0.05, 1)
		assertQtyEquals(cn, 49.9999, 1e-3)
	}
}
