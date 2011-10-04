package starling.eai.instrumentreaders.physical

import starling.utils.StarlingTest
import org.testng.annotations.Test
import org.testng.Assert._
import starling.quantity.Quantity
import starling.quantity.UOM._
import starling.quantity.utils.QuantityTestUtils._
import starling.market.{TestMarketTest, Index, FuturesFrontPeriodIndex, PublishedIndex}

class PricingFormulaParserTests extends TestMarketTest {

//  @Test
//  def testSimple1 {
//    val xml = <tokens>
//      <token>
//        <typeid>2</typeid> <p1id>317</p1id> <cur>USD</cur> <curid>1</curid>
//      </token> <token>
//        <typeid>1</typeid> <op>+</op> <p1>1.81</p1>
//      </token> <token>
//        <typeid>3</typeid> <op>+</op> <p1>Fortis</p1> <p2>0.125</p2>
//      </token>
//    </tokens>
//
//    val formula = PricingFormulaParser.parse(xml.toString, Index.singleIndexFromEAIQuoteID _)
//
//    val price = formula.price(false) {
//      i => {
//        assertEquals(i, Index.PLATTS_BRENT_1ST_MONTH)
//        Quantity(93.69, USD / BBL)
//      }
//    }
//
//    // example taken from trade P383233 on 21Feb11
//    assertQtyEquals(price, Quantity(95.625, USD / BBL), 1e-10)
//  }

//  @Test
//  def testSimple2 {
//    val xml = <tokens>
//      <token>
//        <typeid>2</typeid> <co>0.5</co> <p1id>317</p1id> <cur>USD</cur> <curid>1</curid>
//      </token> <token>
//        <typeid>2</typeid> <op>+</op> <co>0.5</co> <p1id>40</p1id> <cur>USD</cur> <curid>1</curid>
//      </token> <token>
//        <typeid>1</typeid> <op>+</op> <p1>0.32</p1> <fpcur>USD</fpcur>
//      </token> <token>
//        <typeid>3</typeid> <op>+</op> <p1>Ekofisk</p1> <p2>0.95</p2>
//      </token>
//    </tokens>
//
//    val formula = PricingFormulaParser.parse(xml.toString, Index.singleIndexFromEAIQuoteID _)
//    println("formula: " + formula)
//
//    val price = formula.price(false) {
//      case Index.PLATTS_BRENT_1ST_MONTH => {
//        Quantity(102.6, USD / BBL)
//      }
//      case Index.DATED_BRENT => {
//        Quantity(101.85, USD / BBL)
//      }
//    }
//
//    // example taken from trade P392665 on 21Feb11.
//    assertQtyEquals(price, Quantity(103.495, USD / BBL), 1e-8)
//  }
}