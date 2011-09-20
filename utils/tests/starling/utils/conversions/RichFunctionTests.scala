package starling.utils.conversions

import org.scalatest.testng.TestNGSuite
import org.scalatest.matchers.ShouldMatchers
import org.testng.annotations.Test


class RichFunctionTests extends TestNGSuite with ShouldMatchers with RichFunction {
  @Test def canFormConjunctionOfTwoPredicates {
    val conjunction = (startsWithFoo && endsWithBar)

    conjunction("foobar") should be === true
    conjunction("foo") should be === false
    conjunction("bar") should be === false
  }

  @Test def canNegatePredicate {
    val negation = startsWithFoo negate

    negation("foo") should be === false
    negation("") should be === true

    val neg2 = swf negate

    neg2("foo", 3) should be === false
    neg2("", 3) should be === true
  }

  val startsWithFoo: (String) => Boolean = (s: String) => s.startsWith("foo")
  val endsWithBar: (String) => Boolean = (s: String) => s.endsWith("bar")

  val swf: (String, Int) => Boolean = (s: String, i: Int) => startsWithFoo(s)
}