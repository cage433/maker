package starling.utils


import org.scalatest.testng.TestNGSuite
import java.io.File
import org.testng.annotations.Test
import org.testng.Assert._

class StringIOTests extends TestNGSuite {
  @Test
  /**
   *  Test we can write a string to a file and get back the same result
   */
  def testWriteReadToFile{
    val file = File.createTempFile("foo", "bar")
    val text = """
    Some text
      with tabs     , spaces

      and new lines

    """
    StringIO.writeZippedStringToFile(file, text)
    val text2 = StringIO.readZippedStringFromFile(file)
    assertEquals(text, text2)
    file.delete
  }
}
