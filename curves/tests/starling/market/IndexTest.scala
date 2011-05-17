package starling.market

import starling.utils.StarlingTest
import formula.FormulaIndexes
import org.testng.annotations.{AfterTest, Test}
import org.testng.Assert._
import starling.quantity.UOM._
import scala.collection.JavaConversions._
import starling.utils.CaseInsensitive._

class IndexTest extends TestExpiryRules{
  var oldIndexes: Option[FormulaIndexes] = FormulaIndexList.indexes

  FormulaIndexList.set(Some(
    new FormulaIndexes {
      def eaiQuoteMap = Map()
    })
    )

  @AfterTest
  def after3 {
    FormulaIndexList.set(oldIndexes)
  }


  @Test
  def caseInsenstive {
    val testMarket = FuturesMarket.testMarket("market", USD, MT)
    val i1 = PublishedIndex("Index", testMarket)
    val i2 = PublishedIndex("inDex", testMarket)
    assertEquals(i1, i2)

    val s1 = new FuturesSpreadIndex("SpreadIndex", i1, i2, USD, MT, None)
    val s2 = new FuturesSpreadIndex("sPreAdindeX", i1, i2, USD, MT, None)
    assertEquals(s1, s2)
  }

}
