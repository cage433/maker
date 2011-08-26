package starling.utils

import org.scalatest.testng.TestNGSuite
import org.testng.annotations.Test
import starling.utils.Pattern.Extractor
import starling.utils.ImplicitConversions._
import org.testng.Assert._


class ExtractorTest extends TestNGSuite {
  def throwException: Option[Int] = throw new Exception("boom")

  @Test def shouldCatchExceptionsAndReturnNone {
    val ExceptionThrowing: Extractor[String, Int] = Extractor.from[String](str => throwException)

    assertEquals("" partialMatch { case ExceptionThrowing(i) => i}, None)
  }
}