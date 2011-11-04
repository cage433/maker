package starling.market

import starling.utils.StarlingTest
import org.testng.annotations.{AfterTest, Test}
import org.testng.Assert._
import starling.quantity.UOM._
import scala.collection.JavaConversions._
import starling.utils.CaseInsensitive._
import org.scalatest.testng.TestNGSuite

class IndexTest extends TestMarketTest with TestNGSuite{
  @Test
  def caseInsenstive {
    val testMarket = Market.LME_LEAD
    val i1 = PublishedIndex("Index", None, None, MT, USD, null, Lead)
    val i2 = PublishedIndex("inDex", None, None, MT, USD, null, Lead)
    assertEquals(i1, i2)
  }

}
