package starling.utils

import org.scalatest.testng.TestNGSuite
import org.testng.annotations.Test
import org.testng.Assert._
import starling.utils.StringUtils._

class StringUtilsTests extends TestNGSuite {
  @Test
  def testEscapeForCSV {
    assertEquals(escapeForCSV("some \"quoted\" string"), "\"some \"\"quoted\"\" string\"")
    assertEquals(escapeForCSV("some, string"), "\"some, string\"")
    assertEquals(escapeForCSV("some string"), "some string")
    assertEquals(escapeForCSV("some\nstring"), "\"some\nstring\"")
  }
}
