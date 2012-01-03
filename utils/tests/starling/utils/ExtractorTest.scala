package starling.utils

import org.scalatest.testng.TestNGSuite
import org.testng.annotations.Test
import starling.utils.Pattern.Extractor
import starling.utils.ImplicitConversions._
import org.testng.Assert._
import scalaz.Scalaz._


class ExtractorTest extends TestNGSuite {
  def throwException: Option[Int] = throw new Exception("boom")

  @Test def shouldCatchExceptionsAndReturnNone = Log.off {
    val ExceptionThrowing: Extractor[String, Int] = Extractor.from[String](str => throwException)

    assertEquals("" partialMatch { case ExceptionThrowing(i) => i}, None)
  }

  @Test def fromShouldWork = {
    val FooExtractor = Extractor.from[String](str => (str == "foo").option(3))

    assertEquals("foo" partialMatch { case FooExtractor(int) => int }, Some(3))
    assertEquals("bar" partialMatch { case FooExtractor(int) => int }, None)
  }
}