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
import starling.utils.ImplicitConversions._

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
  def almostEq() {
    assertTrue(Qty(6, USD).isAlmostEqual(Qty(5, USD), 1.1))
    assertFalse(Qty(6, USD).isAlmostEqual(Qty(5, USD), .9))
    assertFalse(Qty(6, USD).isAlmostEqual(Qty(6, EUR), 10))
  }

  @Test
  def shouldApplyFixedConversions {
    // dollar and cent
    assertEquals(1 (USD) / 1(US_CENT), (100 (SCALAR)))
    assertEquals(10 (USD*USD / BBL) / 1 (USD/BBL), 10 (USD))
    assertEquals(10(USD * USD) / 1(US_CENT), 1000(USD))
    assertEquals(10(USD * USD *USD) / 1(US_CENT), 1000(USD*USD))
    assertEquals(10(USD * USD *USD) / 1(USD), 10(USD*USD))
    assertEquals(10 (USD*USD / BBL) / 1 (US_CENT/BBL), 1000 (USD))
    assertEquals(256 (US_CENT) in USD, Some(2.56 (USD)))
    assertEquals(2 (USD) + 56 (US_CENT), 2.56 (USD))
    assertEquals(2 (USD) - 56 (US_CENT), 1.44 (USD))
    assertEquals(201 (US_CENT) - 1 (US_CENT), 2 (USD))
    assertEquals(201 (US_CENT) - 1 (US_CENT), 200 (US_CENT))
    assertEquals(2 (USD/BBL) + 56 (US_CENT/BBL), 2.56 (USD/BBL))
    assertEquals(2 (USD/GAL) + 42 (USD/BBL),  3 (USD/GAL))

    // more complicated conversions (checked against wolframalpha.com)
    assertEquals(10 (USD/(G*G)) in (USD/(KG*KG)),  Some(10e6 (USD/(KG*KG))))
    assertQtyOptionClose(10 (USD/(KG*KG)) in (USD/(LB*LB)),  Some(2.057 (USD/(LB*LB))))
    assertEquals(10000 (G*G/USD) in (KG*KG/USD), Some(.01 (KG*KG/USD)))

    // mass and volume
    assertEquals(10 (USD/G) in (USD/KG), Some(10000 (USD/KG)))
    assertEquals(10 (USD/G) / 1 (USD/KG), (10000 (SCALAR)))
    assertEquals(0.01 (MT) in G, Some(10000 (G)))
    assertEquals(100 (L) in M3, Some(0.1 (M3)))
    assertEquals(2.43 (USD/GAL) in (US_CENT/GAL), Some(243 (US_CENT/GAL)))

    // check that it doesn't apply changes it shouldn't have conversions for
    assertEquals(10 (GBP) in USD, None)
    assertEquals(1 (MT) in USD, None)
    assertEquals(1 (MT) in GAL, None)
    assertEquals(1 (BBL) in M3, None)

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
  def shouldConvertChained {
    val bblPerMT = 50
    val map = Map(UOM.BBL / UOM.MT -> BigDecimal(bblPerMT))

    implicit val conv = Conversions(map)

    assertEquals(10 (USD/BBL) in (USD/MT), Some(500 (USD/MT))) // 50 * 10
    assertEquals(10 (USD/BBL) in (USD/KG), 500 (USD/MT) in (USD/KG))

    // and inverted
    assertEquals(100 (USD/MT) in (USD/BBL), Some(2 (USD/BBL))) // 100 / 50
    assertEquals(100 (USD/MT) in (USD/L), 2 (USD/BBL) in (USD/L))

    // and with powers
    assertEquals(100 (USD/(MT*MT)) in (USD/(BBL*BBL)), Some(.04 (USD/(BBL*BBL)))) // 100 / (50*50)
    assertEquals(100 (USD/(MT*MT)) in (USD/(L*L)), .04 (USD/(BBL*BBL)) in (USD/(L*L)))
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

  @Test
  def testBase {
    (1 (KG) inBaseUOM) should be === 1000 (G)
    (1 (MT) inBaseUOM) should be === 1e6 (G)
    (1 (GAL) inBaseUOM) should be === 1 (GAL)
    (1 (BBL) inBaseUOM) should be === 42 (GAL)

    (1000 (USD/KG) inBaseUOM) should be === 1 (USD/G)
    (1000 (US_CENT/KG) inBaseUOM) should be === .01 (USD/G)
  }

  @DataProvider(name = "testNotEquals")
  def testNotEqualsProvider = constructArgs(
    (1 (USD), (1 (GBP))),
    (1 (GAL), (1 (BBL))),
    (1 (USD), (101 (US_CENT)))
  )

  @Test(dataProvider = "testNotEquals")
  def testNotEquals(a: Quantity, b: Quantity) {
    a should not be === (b)
    assertFalse(a.isAlmostEqual(b, .001))
  }

  @DataProvider(name = "testEquals")
  def testEqualsProvider = constructArgs(
    (1(USD), (1(USD))),
    (1(GAL), (1(GAL))),
    (1000(L), (1(KL))),
    (1000(G), (1(KG))),
    (1000(L), (1(KL))),
    (1(USD), (100(US_CENT))),
    (1000(USD/(KG*KG)), (.1(US_CENT/(G*G)))),
    (123.456789(GAL), (2.939447357142857(BBL))),
    (123.456789(OZ), (.0038398553738104024(MT))),
    (1.5(USD), (150(US_CENT)))
  )
  @Test(dataProvider = "testEquals")
  def testEquals(a: Quantity, b: Quantity) {
    a should be === b
    a.hashCode should be === b.hashCode
    assertTrue(a.isAlmostEqual(b, 0))
  }

  @Test def nullIsAdditiveZero = for (zero <- additiveZeros; value <- values) {
    zero + value should be === value.copy()
    value + zero should be === value.copy()
    value - zero should be === value.copy()
  }

  @Test def oneIsMultiplicativeZero = for (one <- multiplicativeZeros; value <- values) {
    one * value should be === value.copy()
    value * one should be === value.copy()
    value / one should be === value.copy()
  }

  @Test
  def testPercent {
    val tenP = Percentage.fromPercentage(10)
    val tenQ = Quantity(10, UOM.PERCENT)
    val oneD = Quantity(1, UOM.USD)

    assertQtyEquals(tenP.toQuantity, tenQ)
    assertEquals(tenP.percentageValue, tenQ.value)

    assertQtyEquals(oneD * tenP, oneD * tenQ)
    assertQtyEquals(oneD * tenQ, Quantity(10, US_CENT))
    assertQtyEquals(oneD / tenQ, Quantity(10, USD))
    assertQtyEquals(tenQ / oneD, Quantity(.1, SCALAR/USD))

    assertQtyEquals(tenQ * tenQ, Quantity(1, PERCENT))
    assertQtyEquals(tenQ + tenQ, Quantity(20, PERCENT))
    assertQtyEquals(tenQ - tenQ, Quantity(0, PERCENT))
  }

  @Test
  def testExtractor {
    List(1.5 (USD/MT), 0.7 (GBP)).foreach {q => 
      println("text = " + q.toString)
      val roundTripQ = q.toString partialMatch {
        case Quantity.Parse(q2) => q2
      } 
      println("Round trip = " + roundTripQ)
      roundTripQ should be === Some(q)
  }
}

  private def values = Quantity(12.34, UOM.USD) :: multiplicativeZeros ::: additiveZeros
  private def additiveZeros = List(Quantity.NULL, Quantity(0, UOM.NULL))
  private def multiplicativeZeros = List(Quantity.ONE, Quantity(1, UOM.SCALAR))
}
