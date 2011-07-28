package starling.quantity

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.testng.TestNGSuite
import org.testng.annotations.Test
import starling.utils.ImplicitConversions._

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
}