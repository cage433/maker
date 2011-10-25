package starling.manual

import starling.LIMServer
import starling.props.PropsHelper
import starling.daterange.Day._
import starling.services.StarlingInit
import starling.db.MarketDataSet._
import starling.auth.AuthHandler
import starling.utils.ThreadUtils
import starling.utils.Broadcaster
import starling.utils.ImplicitConversions._
import starling.db.{MarketDataSnapshots, NewSchemaMdDB, DBMarketDataStore}
import starling.curves.readers.PriceFixingLimMarketDataSource

object RefinedMetalsLimMarketDataSourceTests {
  def main(args:Array[String]) {
    val init = new StarlingInit(PropsHelper.defaultProps, AuthHandler.Dev, Broadcaster.Null, false, false, false, false)

//    val x: Map[PriceFixingsHistoryDataKey, List[CommodityMarket]] = Market.markets.groupBy(PriceFixingsHistoryDataKey.apply(_))
//    x.foreach { case (key, values) => {
//      if (values.size > 1) {
//        println(values)
//      }
//    } }
//
//
//    throw new Exception("DONE")


    val importing = true

    if (importing) {
      init.marketDataStore.importFor(10 Sep 2011, LIM)//LIM, System)
//      val map = init.marketDataStore.marketDataSources(LimMetals).read(25 Apr 2011)
//      val days = map.mapValues(mde => mde.map(_.observationPoint.day)).toSet
//      println("map:: " + map)
//      println("days:: " + days)
//      days.foreach(println)
    }
    else {
      val source = new PriceFixingLimMarketDataSource(
        new LIMServer("ttraflonrh221", 6400), init.businessCalendars, Broadcaster.Null, "from", "to")

      val updates = new DBMarketDataStore(new NewSchemaMdDB(init.starlingRichDB, init.dataTypes),
        new MarketDataSnapshots(init.starlingRichDB), MultiMap(LIM ->> source), Broadcaster.Null, init.dataTypes).importer.getUpdates(12 Sep 2011, LIM)

      updates.get(LIM)
      source.read(12 Apr 2011)
    }

    ThreadUtils.printNonDaemonThreads
  }
}

object LimLiborMarketDataSourceTests {
  def main(args: Array[String]) {
    val init = new StarlingInit(PropsHelper.defaultProps, AuthHandler.Dev, Broadcaster.Null, false, false, false, false)

//    val source = new LimLiborMarketDataSource(new LIMServer("ttraflonrh221", 6400))
//    source.read(20 Apr 2011).foreach(println)
  }
}
