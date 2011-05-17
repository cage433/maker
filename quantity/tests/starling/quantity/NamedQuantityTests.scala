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
    namedAbc.expr should be === "abc" % abc.toString
  }

  @Test def onceANamedQuantityAlwaysANamedQuantity {
    (namedAbc * 2.0)       .expr should be === "(%s * %s)" % (namedAbc.expr, 2.0)
    (namedAbc * percentage).expr should be === "(%s * %s)" % (namedAbc.expr, percentage)
    (namedAbc * abc)       .expr should be === "(%s * %s)" % (namedAbc.expr, abc)
    (namedAbc * namedAbc)  .expr should be === "(%s * %s)" % (namedAbc.expr, namedAbc.expr)
    (namedAbc / 2.0)       .expr should be === "(%s / %s)" % (namedAbc.expr, 2.0)
    (namedAbc / abc)       .expr should be === "(%s / %s)" % (namedAbc.expr, abc)
    (namedAbc / namedAbc)  .expr should be === "(%s / %s)" % (namedAbc.expr, namedAbc.expr)
    (namedAbc + abc)       .expr should be === "(%s + %s)" % (namedAbc.expr, abc)
    (namedAbc + namedAbc)  .expr should be === "(%s + %s)" % (namedAbc.expr, namedAbc.expr)
    (namedAbc - abc)       .expr should be === "(%s - %s)" % (namedAbc.expr, abc)
    (namedAbc - namedAbc)  .expr should be === "(%s - %s)" % (namedAbc.expr, namedAbc.expr)
    (-namedAbc)            .expr should be === "-%s"     % (namedAbc.expr)
    (namedAbc.negate)      .expr should be === "-%s"     % (namedAbc.expr)
    (namedAbc.abs)         .expr should be === "abs(%s)" % (namedAbc.expr)
    (namedAbc.invert)      .expr should be === "(1/%s)"  % (namedAbc.expr)
  }

  @Test def shouldIgnoreOneWhenScaling {
    (namedAbc * 1.0).expr should be === "%s" % namedAbc.expr
    (namedAbc / 1.0).expr should be === "%s" % namedAbc.expr

    (namedAbc * Quantity.ONE).expr should be === "%s" % namedAbc.expr
    (namedAbc / Quantity.ONE).expr should be === "%s" % namedAbc.expr

    (namedAbc * Percentage(1.0)).expr should be === "%s" % namedAbc.expr
    (namedAbc / Percentage(1.0)).expr should be === "%s" % namedAbc.expr
  }

  @Test def shouldDisplaySmallNumbersUsingScientificNotation {
    (namedAbc + new Quantity(0.00499)).expr should be === "(%s + %s)" % (namedAbc.expr, "0.00499")
    (namedAbc + new Quantity(0.000499)).expr should be === "(%s + %s)" % (namedAbc.expr, "4.99\u00D710\u207B\u2074")
    (namedAbc + new Quantity(0.000047)).expr should be === "(%s + %s)" % (namedAbc.expr, "4.7\u00D710\u207B\u2075")
    (namedAbc + new Quantity(0.00004)).expr should be === "(%s + %s)" % (namedAbc.expr, "4.0\u00D710\u207B\u2075")
    (namedAbc + new Quantity(0.000002)).expr should be === "(%s + %s)" % (namedAbc.expr, "2.0\u00D710\u207B\u2076")
    (namedAbc * new Quantity(0.000002, UOM.USD)).expr should be === "(%s * %s)" % (namedAbc.expr, "2.0\u00D710\u207B\u2076 USD")
  }
}