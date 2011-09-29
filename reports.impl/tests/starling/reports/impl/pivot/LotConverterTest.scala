package starling.reports.impl.pivot

import org.testng.annotations._
import starling.quantity.UOM._
import org.scalatest.matchers.ShouldMatchers
import starling.quantity.{Quantity, UOM}
import starling.utils.{ScalaTestUtils, StarlingTest}
import ScalaTestUtils._
import org.scalatest.testng.TestNGSuite

class LotConverterTest extends TestNGSuite with ShouldMatchers {

  @DataProvider(name = "uomProvider")
  def uomProvider = constructArgs(
    (BBL, K_BBL),
    (MT, K_MT),
    (M3, C_M3),
    ((BBL ^ 2) / USD, ((K_BBL ^ 2) / USD))
  )

  @Test(dataProvider = "uomProvider")
  def shouldConvertUOMs(from : UOM, to : UOM) = {
    LotConverter.convert(from) should be === to
  }


  @DataProvider(name = "quantityProvider")
  def quantityProvider = constructArgs(
    (Quantity(1000, BBL), Quantity(1, K_BBL)),
    (Quantity(1000, GAL), Quantity(1, K_GAL)),
    (Quantity(10000, (BBL ^ 2) / USD), Quantity(.01, (K_BBL ^ 2) / USD))
  )

  @Test(dataProvider = "quantityProvider")
  def shouldConvertQuantities(from : Quantity, to : Quantity) = {
    LotConverter.convert(from) should be === to
  }
}

