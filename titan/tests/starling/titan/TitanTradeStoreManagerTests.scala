package starling.titan

import starling.utils.StarlingTest
import org.testng.annotations.Test
import starling.richdb.RichDB
import starling.daterange.Day
import starling.marketdata.PriceDataKey
import starling.curves.{ForwardPriceKey, UnitTestingEnvironment}
import starling.quantity.Quantity
import starling.quantity.UOM._
import starling.market.TestMarketSpec
import starling.instrument.Trade


class TitanTradeStoreManagerTests extends TestMarketSpec{

//  @Test
//  def testValuationChangedMakesEvent{
    val tradeStore = new TitanTradeStore(null, null, null, null){
      val tradesByTitanId = Map[String, List[Trade]]()
      override def getTradesForTitanTradeID(titanTradeID: String) = tradesByTitanId.getOrElse(titanTradeID, Nil)
    }

    val titanServiceCache = new TitanServiceCache(null, null, null){
      override def updateTrade(titanTradeID: String) = null
    }

    val ttsm = new TitanTradeStoreManager(titanServiceCache, tradeStore){

    }
    val env = UnitTestingEnvironment(
      Day(2011, 11, 19).endOfDay,
      {
        case _ : ForwardPriceKey => Quantity(1, USD / MT)
      }
    )
    ttsm.updateTrade(env, "1", "Event 1")
    
//  }
}

object TitanTradeStoreManagerTests extends App{
  new TitanTradeStoreManagerTests()
}