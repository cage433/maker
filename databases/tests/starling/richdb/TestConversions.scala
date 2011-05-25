package starling.richdb

import org.testng.annotations.Test
import starling.daterange.Day
import scala.collection.JavaConversions._
import org.testng.Assert._
import java.sql.Date
import starling.db.DBConvert

class TestConversions {

  @Test
  def testConversions {
    import RichConversions._

    val map = Map("period_3" -> Day(2010, 1, 1), "Period" -> Day(2010, 1, 2), "NotPeriod" -> Day(2011, 1, 1))

    val converted = convertTypes(map,  DBConvert.convertTypes _)
    // "period" is a varchar in our tables so we can't have day converting into a sql date or it will break on the query
    assertEquals(converted("period_3").getClass, classOf[String])
    assertEquals(converted("period_3"), Day(2010, 1, 1).toString)
    assertEquals(converted("Period").getClass, classOf[String])
    assertEquals(converted("Period"), Day(2010, 1, 2).toString)
    // however we do want non-"period" columns to be converted
    assertEquals(converted("NotPeriod").getClass, classOf[Date])
    assertEquals(converted("NotPeriod"), Day(2011, 1, 1).toSqlDate)

  }
}
