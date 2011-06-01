package starling.quantity


import org.scalatest.testng.TestNGSuite
import org.testng.Assert._
import starling.quantity.UOM._
import starling.utils.CaseInsensitive._
import org.scalatest.matchers.ShouldMatchers
import org.testng.annotations.{DataProvider, Test}
import starling.utils.ScalaTestUtils
import ScalaTestUtils._

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
    assertEquals((USD * 1000).toString, "K USD")
    assertEquals( KG.toString, "K g")
    assertEquals( (CNY / KG).toString, "CNY/K g")
  }

  @Test
  def testPower {
    assertEquals(USD^2, USD * USD)
    assertEquals(USD/EUR * USD/EUR, (USD/EUR)^2)
    assertEquals(USD ^ (-1), USD.inverse)
    assertEquals((USD/EUR)^(-1), EUR/USD)
  }

  @Test
  def testFromString {
    assertEquals(UOM.getSymbol("RMB"), UOMSymbol.CNY_SYMBOL)
    assertEquals(UOM.getSymbol("CNY"), UOMSymbol.CNY_SYMBOL)
    assertEquals(UOM.getSymbol("GRAM"), UOMSymbol.GRAM_SYMBOL)
    assertEquals(UOM.getSymbol("g"), UOMSymbol.GRAM_SYMBOL)
    assertEquals(UOM.getSymbol("G"), UOMSymbol.GRAM_SYMBOL)
    assertEquals(UOM.getSymbol("gRaM"), UOMSymbol.GRAM_SYMBOL)

    assertEquals(UOM.fromString("RmB/Gram"), UOM.CNY/UOM.G)
    assertEquals(UOM.fromString("G/USD"), UOM.G/UOM.USD)
    assertEquals(UOM.fromString("LB/G"), UOM.LB/UOM.G)
    assertEquals(UOM.fromString("K USD"), UOM.USD * 1000)
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
  def testReplaceUOM{
    assertEquals((USD * USD / MT).replace(USD, EUR), EUR * EUR / MT)
    assertEquals((USD * USD / MT).replace(MT, BBL), USD * USD / BBL)
    assertEquals((USD / (MT^2)).replace(USD/MT, USD/BBL), USD / (MT * BBL))
    assertEquals((MT * MT * USD * BBL / (USD * USD * LB)).replace(USD / MT, EUR / LB), MT * BBL / EUR)
  }

  def convert(from : UOM, to : UOM): Option[Double] = Conversions.default.convert(from, to)

  @DataProvider(name = "conversions")
  def conversions = constructArgs(
    (UOM.GBP,        UOM.GBP, 1.0),
    (UOM.GBP * 1000, UOM.GBP, 1000.0)
  )

  @Test(dataProvider = "conversions")
  def testConversions(from : UOM, to : UOM, expected : Double) = convert(from, to) should be === Some(expected)

  @Test
  def shouldProceesScalesAlgebraically {
    val uom = UOM.GBP * 1000
    val first = ((UOM.GBP * UOM.BBL * (11 * 7)) / (UOM.AUD * UOM.MT))
    val second = ((UOM.GBP * (11 * 13) * UOM.BUSHEL_SOY) / (UOM.AUD * UOM.COMEX_SILVER_LOTS))

    (uom / uom).scale should be === uom.scale / uom.scale
    (uom * uom).scale should be === uom.scale * uom.scale
    (uom ^ 4).scale should be === (uom.scale ^ 4)
    uom.inverse.scale should be === uom.scale.inverse
    first.gcd(second).scale should be === first.scale.gcd(second.scale)
  }

  @Test
  def shouldBeAbleToBreakApartAndReconstructNumeratorsAndDenominators
  {
    val uom = (UOM.BBL * 13) / (UOM.USD * 7)

    uom.numeratorUOM / uom.denominatorUOM should be === uom
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
}
