package starling.quantity


import org.scalatest.testng.TestNGSuite
import org.testng.Assert._
import starling.quantity.UOM._
import starling.utils.CaseInsensitive._
import org.scalatest.matchers.ShouldMatchers
import org.testng.annotations.{DataProvider, Test}
import starling.utils.ScalaTestUtils
import ScalaTestUtils._
import starling.utils.ImplicitConversions._

class UOMTests extends TestNGSuite with ShouldMatchers {
  @Test
  def testToString {
    def assEq(a : UOM, b : UOM) {
      assertEquals(a, b)
      assertEquals(a.toString, b.toString)
    }

    assEq(EUR * USD / (MT * MT * OZ),
      USD * EUR/ (OZ * MT * MT))

    assEq((MT / USD).inverse, USD / MT)
    assEq(USD * MT / USD * MT, MT^2)

    assertEquals((MT^2).toString, "MT^2")
    assertEquals((MT.inverse * MT).toString, "")
    assertEquals((USD / MT * EUR / (OZ * MT)).toString, "EURUSD/MT^2oz")
    assertEquals( KG.toString, "kg")
    assertEquals( (CNY / KG).toString, "CNY/kg")
    assertEquals( (SCALAR / USD).toString, "USD^-1")
    assertEquals( (SCALAR / (USD*USD)).toString, "USD^-2")
  }

  @Test
  def testPower {
    assertEquals(USD^2, USD * USD)
    assertEquals(USD^3, USD * USD * USD)
    assertEquals(USD/EUR * USD/EUR, (USD/EUR)^2)
    assertEquals(USD ^ (-1), USD.inverse)
    assertEquals((USD/EUR)^(-1), EUR/USD)
  }

  @Test
  def testNumDen {
    assertEquals(NULL.numeratorUOM, NULL)
    assertEquals(NULL.denominatorUOM, SCALAR) // not sure why but matches old logic

    assertEquals((USD/(EUR*EUR)).denominatorUOM, EUR*EUR)
    assertEquals((USD / EUR).denominatorUOM, EUR)
    assertEquals((USD/EUR).numeratorUOM, USD)

    assertEquals((EUR.inverse).numeratorUOM, SCALAR)
    assertEquals((EUR.inverse).denominatorUOM, EUR)
    assertEquals((SCALAR/EUR).numeratorUOM, SCALAR)
    assertEquals((EUR).denominatorUOM, SCALAR)
    
    assertEquals((USD*USD/EUR).numeratorUOM, USD*USD)
    assertEquals(((USD*USD)/(USD*MT)).numeratorUOM, USD)
    assertEquals(((USD*USD)/(USD*MT)).denominatorUOM, MT)
    assertEquals(((US_CENT*US_CENT)/(US_CENT*MT)).numeratorUOM, US_CENT)
  }

  @Test
  def testFromString {
    assertEquals(UOM.fromString("Gram"), UOM.G)
    assertEquals(UOM.fromString("RmB"), UOM.CNY)

    assertEquals(UOM.fromString("G/USD"), UOM.G/UOM.USD)
    assertEquals(UOM.fromString("LB/L"), UOM.LB/UOM.L)
    assertEquals(UOM.fromString("RmB/Gram"), UOM.CNY/UOM.G)
    assertEquals(UOM.fromString(UOMSymbol.US_CENT_SYMBOL.name), UOM.US_CENT)
    //    assertEquals(UOM.fromString("K USD"), UOM.USD * 1000)
  }

  @Test
  def uomSymbolsShouldBeUnique {
    (Set.empty[String] /: UOMSymbol.symbols)((set, symbol) => {
      (set /: symbol.names)((set, name) => {
        assertTrue(!set.contains(name), "Symbol name `" + name + "' is not unique.")
        set + name
      })
    })
  }

  @Test
  def testReplaceUOM {
    assertEquals((USD * USD / MT).replace(USD, EUR), EUR * EUR / MT)
    assertEquals((USD * USD / MT).replace(USD, US_CENT), US_CENT * US_CENT / MT)
    assertEquals((USD * USD / MT).replace(MT, BBL), USD * USD / BBL)
    assertEquals((USD / (MT^2)).replace(USD/MT, USD/BBL), USD / (MT * BBL))
    assertEquals((MT * MT * USD * BBL / (USD * USD * THERMS)).replace(USD / MT, EUR / THERMS), MT * BBL / EUR)
  }

//  def convert(from : UOM, to : UOM): Option[BigDecimal] = Conversions.default.convert(from, to)

//  @DataProvider(name = "conversions")
//  def conversions = constructArgs(
//    (UOM.GBP,        UOM.GBP, 1.0),
//    (UOM.USD, UOM.US_CENT, 100.0)
//  )
//
//  @Test(dataProvider = "conversions")
//  def testConversions(from : UOM, to : UOM, expected : Double) = convert(from, to) should be === Some(expected)
//
  @Test
  def shouldBeAbleToBreakApartAndReconstructNumeratorsAndDenominators
  {
//    val uom = (UOM.BBL * 13) / (UOM.USD * 7)
//
//    uom.numeratorUOM / uom.denominatorUOM should be === uom
  }

  @Test
  def shouldDisplayCurrencyConversionRatesUnambiguously {
    val gbpPerEur = Quantity(0.87, UOM.GBP / UOM.EUR)

    gbpPerEur.toString should be === "0.87 GBP per EUR"
  }

  @Test
  def parseFX {
    UOM.GBP.isFX should be === false
    (UOM.GBP / UOM.USD).isFX should be === true

    UOM.fromIdentifier("EUR/GBP") should be === UOM.EUR / UOM.GBP
    UOM.fromIdentifier("EUR per GBP") should be === UOM.EUR / UOM.GBP
  }

  @DataProvider(name = "symbolMapConversionSupplier")
  def symbolMapConversionSupplier() = constructDataProviderArgs(
    List(USD * GBP, USD, UOM.NULL, GBP / MT, GBP * GBP, USD / (GBP * GBP), (GBP ^ 2) * USD / (MT ^ 3))
  )

  @Test(dataProvider = "symbolMapConversionSupplier")
  def testSymbolMapConversion(uom : UOM){
    assertEquals(UOM.fromSymbolMap(uom.asSymbolMap), uom)
  }

  @Test def extractorTest {
    List("USD", "MT", "USD/MT", "%").flatMap { _ partialMatch { case UOM.Parse(uom) => uom } } should be ===
      List(UOM.USD, UOM.MT, UOM.USD / UOM.MT, UOM.PERCENT)
  }

  @Test
  def testBase {
    assertTrue (USD.isBaseUnit)
    assertTrue (USD.inBaseUnit == USD)
    assertTrue (USD.isBaseCurrency)
    assertTrue (USD.inBaseCurrency == USD)

    assertFalse (US_CENT.isBaseUnit)
    assertTrue (US_CENT.inBaseUnit == USD)
    assertFalse (US_CENT.isBaseCurrency)
    assertTrue (US_CENT.inBaseCurrency == USD)

    assertFalse(MT.isBaseUnit)
    assertTrue(G.isBaseUnit)
    assertFalse(G.isBaseCurrency)
    assertTrue(MT.inBaseUnit == G)
  }
}
