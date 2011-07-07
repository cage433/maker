package starling.daterange

import org.scalatest.testng.TestNGSuite
import org.testng.annotations.Test
import org.testng.Assert._
import starling.daterange.Day._

class BOMTests extends TestNGSuite {

  @Test
  def testContaining {
    val range = new SimpleDateRange(14 Jan 2011, 31 Jan 2011)
    range.foreach((day) => {
      val containing = BOM.containing(day)
      assertEquals(BOM(day), containing)
      assertEquals(BOM(day).asDateRange, DateRange(day, day.containingMonth.lastDay))
    })
    val bom = BOM(14 Jan 2011)
    assertNotSame(bom, BOM.containing(13 Jan 2011))
    assertNotSame(bom, BOM.containing(15 Jan 2011))
    assertNotSame(bom, BOM.containing(1 Feb 2011))
  }

  @Test
  def testToString {
    assertEquals(BOM(14 Jan 2011).toString, "BOM(14Jan2011)")
  }

  @Test
  def shouldParse {
    assertEquals(BOM.parse("BOM(14Jan2011)"), BOM(14 Jan 2011))
    assertEquals(BOM.parse("BOM(14 Jan 2011)"), BOM(14 Jan 2011))
    assertEquals(BOM.parse("BOM(2011-01-14)"), BOM(14 Jan 2011))
    assertEquals(BOM.parse("BOM(14/01/2011)"), BOM(14 Jan 2011))
    assertEquals(BOM.parse("BOM(14/Jan/2011)"), BOM(14 Jan 2011))
  }
}
