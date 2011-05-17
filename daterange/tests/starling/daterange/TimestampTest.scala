package starling.daterange

import org.scalatest.testng.TestNGSuite
import org.testng.annotations.Test
import org.testng.Assert._

class TimestampTest extends TestNGSuite {

  @Test
  def parse {
    val timestamp = new Timestamp(1259680418251L)

    assertEquals(timestamp, Timestamp.parse(timestamp.toString))
  }
  
  @Test
  def testMaxTimestamp {
    val t1 = Day(2011, 1, 19).toTimestamp
    val t2 = Day(2011, 1, 20).toTimestamp
    val listOfTimestamps = List(t1,t2)
    val maxTimestamp = listOfTimestamps.max
    assertEquals(maxTimestamp, t2)
  }
}
