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

  @Test
  def testFuturesMarketToIndexMap {
    val futuresMarketToIndexMap = Map(
        Market.ICE_BRENT -> Index.BRT11,
        Market.ICE_GAS_OIL -> Index.GO11,
        Market.NYMEX_GASOLINE -> Index.RBOB10,
        Market.NYMEX_HEATING -> Index.HO10,
        Market.NYMEX_WTI -> Index.WTI10,
        Market.ICE_WTI -> Index.ICEWTI10
      )
    futuresMarketToIndexMap.map {
      case (m, i) => {
        assertEquals(Index.futuresMarketToIndex(m), Some(i))
      }
    }
  }
}
