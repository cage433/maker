package starling.manual

import starling.LIMServer
import starling.props.PropsHelper
import starling.daterange.Day._
import starling.services.StarlingInit
import starling.curves.readers.RefinedMetalsLimMarketDataSource
import starling.db.DBMarketDataStore
import starling.db.MarketDataSet._
import starling.auth.AuthHandler
import starling.utils.ThreadUtils
import starling.utils.Broadcaster
import starling.utils.ImplicitConversions._


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
      init.marketDataStore.importFor(10 May 2011, LimMetals)//LIM, System)
//      val map = init.marketDataStore.marketDataSources(LimMetals).read(25 Apr 2011)
//      val days = map.mapValues(mde => mde.map(_.observationPoint.day)).toSet
//      println("map:: " + map)
//      println("days:: " + days)
//      days.foreach(println)
    }
    else {
      val source = new RefinedMetalsLimMarketDataSource(new LIMServer("ttraflonrh221", 6400))
      val updates = DBMarketDataStore(init.props, init.starlingRichDB, MultiMap(LIM ->> source))
        .importer.getUpdates(12 Apr 2011, LIM)

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