package starling.pivot

import org.scalatest.testng.TestNGSuite
import org.testng.annotations._
import org.testng.Assert._
import starling.quantity.Quantity
import starling.quantity.UOM._

class PivotQuantityParserTest extends TestNGSuite {
  val efi = PivotFormatter.DefaultExtraFormatInfo

  @Test
  def testJustNumbersParse() {
    val s1 = "20"
    val expected1 = (PivotQuantity(20.0), "")
    val result1 = PivotQuantityPivotParser.parse(s1, efi)
    assertEquals(result1, expected1)

    val s2 = "20,000"
    val expected2 = (PivotQuantity(20000), "")
    val result2 = PivotQuantityPivotParser.parse(s2, efi)
    assertEquals(result2, expected2)

    val s3 = "25,000.001"
    val expected3 = (PivotQuantity(25000.001), "")
    val result3 = PivotQuantityPivotParser.parse(s3, efi)
    assertEquals(result3, expected3)

    val s4 = "(20)"
    val expected4 = (PivotQuantity(-20.0), "")
    val result4 = PivotQuantityPivotParser.parse(s4, efi)
    assertEquals(result4, expected4)

    val s5 = "-20"
    val expected5 = (PivotQuantity(-20.0), "")
    val result5 = PivotQuantityPivotParser.parse(s5, efi)
    assertEquals(result5, expected5)

    val s6 = "-28.35"
    val expected6 = (PivotQuantity(-28.35), "")
    val result6 = PivotQuantityPivotParser.parse(s6, efi)
    assertEquals(result6, expected6)
  }

  @Test
  def testParseSpotFX() {
    val s1 = "1.55 USD per GBP"
    val expected1 = (PivotQuantity(new Quantity(1.55, USD / GBP)), "1.5500 USD per GBP")
    val result1 = PivotQuantityPivotParser.parse(s1, efi)
    assertEquals(result1, expected1)

    val s2 = "1.552569 USD per GBP"
    val expected2 = (PivotQuantity(new Quantity(1.552569, USD / GBP)), "1.5526 USD per GBP")
    val result2 = PivotQuantityPivotParser.parse(s2, efi)
    assertEquals(result2, expected2)

    val s3 = "1.552569USD per GBP"
    val expected3 = (PivotQuantity(new Quantity(1.552569, USD / GBP)), "1.5526 USD per GBP")
    val result3 = PivotQuantityPivotParser.parse(s3, efi)
    assertEquals(result3, expected3)

    val s4 = "1.552569USDperGBP"
    val expected4 = (PivotQuantity(new Quantity(1.552569, USD / GBP)), "1.5526 USD per GBP")
    val result4 = PivotQuantityPivotParser.parse(s4, efi)
    assertEquals(result4, expected4)
  }

  @Test
  def testParseSingleUOM() {
    val s1 = "1.55 USD"
    val expected1 = (PivotQuantity(new Quantity(1.55, USD)), "2 USD")
    val result1 = PivotQuantityPivotParser.parse(s1, efi)
    assertEquals(result1, expected1)

    val s2 = "1.55USD"
    val expected2 = (PivotQuantity(new Quantity(1.55, USD)), "2 USD")
    val result2 = PivotQuantityPivotParser.parse(s2, efi)
    assertEquals(result2, expected2)
  }
}