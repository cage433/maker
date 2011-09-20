package starling.quantity

import org.scalatest.testng.TestNGSuite
import org.testng.annotations._
import org.testng.Assert._
import starling.utils.ScalaTestUtils
import starling.utils.ScalaTestUtils._
import starling.quantity.{Quantity => Qty}
import starling.quantity.utils.QuantityTestUtils._
import org.scalatest.matchers.ShouldMatchers
import java.lang.String

class QuantityTests extends TestNGSuite with ShouldMatchers {

import starling.quantity.Ratio._
import starling.quantity.UOM._
import starling.quantity.UOMSymbol
import starling.quantity.RichQuantity._

  @Test
  def testQuantityArithmetic{
  	assertQtyEquals(Qty(1.0, USD) + Qty(3.5, USD), Qty(4.5, USD), 1e-9)
  	assertQtyEquals(Qty(1.0, USD) - Qty(3.5, USD), Qty(-2.5, USD), 1e-9)
  	assertQtyEquals(Qty(6.0, USD) / Qty(3.0, USD), new Qty(2.0), 1e-9)
  	assertQtyEquals(Qty(6.0, USD) / Qty(3.0, MT), new Qty(2.0, USD / MT), 1e-9)
    assertQtyEquals(Qty(6.0, USD) * Qty(3.0, MT), new Qty(18.0, USD * MT), 1e-9)
    assertQtyEquals(Qty(6.0, USD) * 2.0, new Qty(12.0, USD), 1e-9)
    assertQtyEquals(-Qty(6.0, USD), new Qty(-6.0, USD), 1e-9)
  	try{
  	  Qty(6, USD) + Qty(5, EUR)
  	  fail
  	} catch {
  	  case e : IllegalStateException => ()
  	}
  }

  @Test
  def testUOMDivision{
    assertEquals(GBP, USD / (USD / GBP))
    assertEquals( GBP * GBP / USD, GBP / (USD / GBP))
    assertNotSame(GBP / USD, USD)
  }
  
  @Test 
  def testEuclidGCD{
    assertEquals(1, gcd(1, 1).toInt)
    assertEquals(1, gcd(3, 5).toInt)
    assertEquals(3, gcd(3 * 3, 3 * 7).toInt)
    assertEquals(3 * 7 * 7, gcd(3 * 3 * 5 * 7 * 7, 3 * 7 * 7 * 7 * 7 * 101).toInt)
  }

  @Test
  def testPrimes{
    assertEquals(List(2, 3, 5, 7, 11, 13, 17, 19, 23), Primes.firstNPrimes(9))
  }
  
  @Test
  def almostEq() {
    assertTrue(Qty(6, USD).isAlmostEqual(Qty(5, USD), 1.1))
    assertFalse(Qty(6, USD).isAlmostEqual(Qty(5, USD), .9))
    assertFalse(Qty(6, USD).isAlmostEqual(Qty(6, EUR), 10))
  }

  @Test
  def shouldApplyFixedConversions {
    assertEquals(256 (US_CENT) in USD, Some(2.56 (USD)))
    assertEquals(0.01 (MT) in G, Some(10000 (G)))
    assertEquals(100 (L) in M3, Some(0.1 (M3)))

    // check that it doesn't apply changes it shouldn't have conversions for
    assertEquals(10 (GBP) in USD, None)
    assertEquals(1 (MT) in USD, None)
    assertEquals(1 (MT) in GAL, None)

    // check that conversions via third units also work
    assertEquals(1 (M3) in KL, Some(1 (KL)))

    // check that compound units also work
    assertEquals(10.0 (US_CENT/MT) in (USD/MT), Some(0.1 (USD/MT)))
    assertEquals(1.0 (USD/MT) in (US_CENT/MT), Some(100.0 (US_CENT/MT)))
    assertQtyOptionClose(31.1034768 (USD/OZ) in (USD/LB), Some(453.59237 (USD/LB)))

    // note: at the moment it doesn't seem like it can do conversions via third units on
    // compount units. hopefully this will work in the future.
    assertQtyOptionClose(31.1034768 (USD/OZ) in (US_CENT/LB), Some(45359.237 (US_CENT/LB)))
    assertQtyOptionClose(1.0 (KL^2) in (L^2), Some(1000000.0 (L^2)))
    assertQtyOptionClose(1000.0 (OZ/GAL) in (LB/BBL), Some(2880.0 (LB/BBL)))
    assertQtyOptionClose(1.0 (USD/GAL) in (USD/BBL), Some(42.0 (USD/BBL)))
  }

  @Test
  def testCompare {
    assertTrue(Qty(2.0, GBP) >= 0)
    assertTrue(Qty(-2.0, GBP) < 0)
    assertTrue(Qty(0.0, GBP) <= 0)
  }

  @Test
  def testCompareReturningZeroIsEqual {
    val q1 = Qty(200.0, GBP)
    val q2 = Qty(200.0, GBP)
    assertEquals(q1, q2)
    assert(q1.compare(q2) == 0)

    val q3 = Qty(199.0, GBP)
    val q4 = Qty(200.0, GBP)

    assertFalse(q3 == q4)
    assertFalse(q3.compare(q4) == 0)
    assertTrue(q3 < q4)
    assertFalse(q4 < q3)
    assertTrue(q4 > q3)
  }

  @Test(expectedExceptions = Array(classOf[AssertionError]))
  def compareDifferentUOMS {
    val q3 = Qty(199.0, GBP)
    val q4 = Qty(200.0, BBL)

    q3.compareTo(q4)
    assert(false, "failed")
  }

  @Test
  def testRounding = {
    assertQtyEquals(123.0725(USD).round(3), 123.073(USD))
    assertQtyEquals(123.072499(USD).round(3), 123.072(USD))
  }


  @Test
  def uomScaling {
    val unscaled = Qty(1000, GBP)
    val scaled = unscaled in (GBP * 1000)

    scaled should be === Some(Qty(1, GBP * 1000))
  }

  @Test
  def optionQuantityShouldBeNumeric {
    val none = None : Option[Quantity]

    List(none, Some(2.0(BBL)), Some(3.0(BBL))).sum should be === Some(5.0(BBL))
    List(none, Some(2.0(BBL)), Some(3.0(BBL))).product should be === Some(6.0(BBL * BBL))
  }

  @Test
  def testPlus {
    val q1 = 1 (USD)
    val q2 = 10 (US_CENT)

    q1 + q2 should be === 1.10 (USD)
    q1 - q2 should be === .90 (USD)

    q2 + q1 should be === 110 (US_CENT)
    q2 - q1 should be === -(90) (US_CENT)
  }

  @Test
  def testFormat {
    19.05 (USD) format("###.#") should be === "19.1 USD"
    (-19.05) (USD) format("###.#") should be === "(19.1) USD"

    new Quantity(1.23).toString should be === "1.23"
  }

  @Test
  def testPercentageDifference {
    val q1 = 100 (USD)
    val zero = 0 (USD)

    q1.percentageDifference(q1) should be === Percentage(0)
    q1.percentageDifference(q1 * 2) should be === Percentage(1)
    q1.percentageDifference(zero) should be === Percentage(1)
    zero.percentageDifference(zero) should be === Percentage(0)
  }

  @Test
  def testRegex {
    val original = "1,2.3 GBP/EUR"
    val parsed = original match {
      case Quantity.Regex(value, unit) => value + " " + unit
    }

    parsed should be === original
  }
}
