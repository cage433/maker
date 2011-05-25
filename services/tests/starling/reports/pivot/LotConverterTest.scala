package starling.reports.pivot

import org.testng.annotations._
import starling.quantity.UOM._
import org.scalatest.matchers.ShouldMatchers
import starling.quantity.{Quantity, UOM}
import starling.utils.{ScalaTestUtils, StarlingTest}
import ScalaTestUtils._

class LotConverterTest extends StarlingTest with ShouldMatchers {

  @DataProvider(name = "uomProvider")
  def uomProvider = constructArgs(
    (BBL, K_BBL),
    (MT, K_MT),
    (M3, C_M3),
    (USD, USD * 1000),
    ((BBL ^ 2) / USD, ((BBL ^ 2) / USD) * 1000)
  )

  @DataProvider(name = "quantityProvider")
  def quantityProvider = constructArgs(
    (Quantity(1000, BBL), Quantity(1, K_BBL)),
    (Quantity(1000, USD), Quantity(1, USD * 1000))
  )

  @Test(dataProvider = "uomProvider")
  def shouldConvertUOMs(from : UOM, to : UOM) = LotConverter.convert(from) should be === to

  @Test(dataProvider = "quantityProvider")
  def shouldConvertQuantities(from : Quantity, to : Quantity) = LotConverter.convert(from) should be === to
}

