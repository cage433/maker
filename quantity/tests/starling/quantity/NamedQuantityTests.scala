package starling.quantity

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.testng.TestNGSuite
import org.testng.annotations.Test
import starling.utils.ImplicitConversions._
import org.testng.Assert._

class NamedQuantityTests extends TestNGSuite with ShouldMatchers {
  val abc = new Quantity(123.45)
  val namedAbc = abc.named("abc")
  val xyz = new Quantity(456.78)
  val namedXyz = xyz.named("xyz")

  val percentage = Percentage(23.45)

  @Test def toStringShouldIncludeName {
    namedAbc.format(0) should be === "abc"
  }

  @Test def onceANamedQuantityAlwaysANamedQuantity {
    (namedAbc * 2.0)       .format(0) should be === "(%s * %s)" % (namedAbc.format(0), 2.0)
    (namedAbc * percentage).format(0) should be === "(%s * %s)" % (namedAbc.format(0), percentage)
    (namedAbc * abc)       .format(0) should be === "(%s * %s)" % (namedAbc.format(0), abc)
    (namedAbc * namedAbc)  .format(0) should be === "(%s * %s)" % (namedAbc.format(0), namedAbc.format(0))
    (namedAbc / 2.0)       .format(0) should be === "(%s / %s)" % (namedAbc.format(0), 2.0)
    (namedAbc / abc)       .format(0) should be === "(%s / %s)" % (namedAbc.format(0), abc)
    (namedAbc / namedAbc)  .format(0) should be === "(%s / %s)" % (namedAbc.format(0), namedAbc.format(0))
    (namedAbc + abc)       .format(0) should be === "(%s + %s)" % (namedAbc.format(0), abc)
    (namedAbc + namedAbc)  .format(0) should be === "(%s + %s)" % (namedAbc.format(0), namedAbc.format(0))
    (namedAbc - abc)       .format(0) should be === "(%s - %s)" % (namedAbc.format(0), abc)
    (namedAbc - namedAbc)  .format(0) should be === "(%s - %s)" % (namedAbc.format(0), namedAbc.format(0))
    (-namedAbc)            .format(0) should be === "-%s"     % (namedAbc.format(0))
    (namedAbc.negate)      .format(0) should be === "-%s"     % (namedAbc.format(0))
    (namedAbc.abs)         .format(0) should be === "abs(%s)" % (namedAbc.format(0))
    (namedAbc.invert)      .format(0) should be === "(1/%s)"  % (namedAbc.format(0))
  }

  @Test def shouldIgnoreOneWhenScaling {
    (namedAbc * 1.0).format(0) should be === namedAbc.format(0)
    (namedAbc / 1.0).format(0) should be === namedAbc.format(0)

    (namedAbc * Quantity.ONE).format(0) should be === namedAbc.format(0)
    (namedAbc / Quantity.ONE).format(0) should be === namedAbc.format(0)

    (namedAbc * Percentage(1.0)).format(0) should be === namedAbc.format(0)
    (namedAbc / Percentage(1.0)).format(0) should be === namedAbc.format(0)
  }

  @Test def shouldDisplaySmallNumbersUsingScientificNotation {
    (namedAbc + new Quantity(0.00499)).format(0) should be === "(%s + %s)" % (namedAbc.format(0), "0.00499")
    (namedAbc + new Quantity(0.000499)).format(0) should be === "(%s + %s)" % (namedAbc.format(0), "4.99\u00D710\u207B\u2074")
    (namedAbc + new Quantity(0.000047)).format(0) should be === "(%s + %s)" % (namedAbc.format(0), "4.7\u00D710\u207B\u2075")
    (namedAbc + new Quantity(0.00004)).format(0) should be === "(%s + %s)" % (namedAbc.format(0), "4.0\u00D710\u207B\u2075")
    (namedAbc + new Quantity(0.000002)).format(0) should be === "(%s + %s)" % (namedAbc.format(0), "2.0\u00D710\u207B\u2076")
    (namedAbc * new Quantity(0.000002, UOM.USD)).format(0) should be === "(%s * %s)" % (namedAbc.format(0), "2.0\u00D710\u207B\u2076 USD")
  }

  @Test
  def testSimpleNamedQuantityLevels() {
    val q1 = SimpleNamedQuantity("1", Quantity(1, UOM.USD))
    val q2 = SimpleNamedQuantity("2", q1)
    assertEquals(q2.name, "2")
    assertEquals(q2.format(0), "2")
    assertEquals(q2.format(1), "1")
    assertEquals(q2.format(2), "1.00 USD")
  }

  @Test
  def testBinOpNamedQuantityLevels() {
    val q1 = SimpleNamedQuantity("one", Quantity(1, UOM.USD))
    val q2 = SimpleNamedQuantity("two", Quantity(2, UOM.USD))
    val mult = q1 * q2
    val expectMult = BinOpNamedQuantity("*", q1, q2, q1 * q2)
    assertEquals(mult, expectMult)

    assertEquals(mult.name, "(one * two)")
    assertEquals(mult.format(1), "(1.00 USD * 2.00 USD)")
    assertEquals(mult.format(2), "(1.00 USD * 2.00 USD)")
  }

  @Test
  def testFunctionNamedQuantity() {
    val q1 = SimpleNamedQuantity("one", Quantity(1, UOM.USD))
    val q2 = SimpleNamedQuantity("two", Quantity(2, UOM.USD))
    val result = FunctionNamedQuantity("SUM", List(q1, q2), q1 + q2)
    assertEquals(result.name, "SUM(one, two)")
    assertEquals(result.format(1), "SUM(1.00 USD, 2.00 USD)")
    assertEquals(result.format(2), "SUM(1.00 USD, 2.00 USD)")
  }


}