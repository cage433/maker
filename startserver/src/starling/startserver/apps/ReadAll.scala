package starling.startserver.apps

import starling.utils.Log
import starling.db.MarketDataStore
import starling.rmi.UserSettingsDatabase
import starling.tradestore.TradeStores


object ReadAll {

  def main(args : Array[String]) {
    BaseRunner.runWithoutListening() { lookup => {
      lookup(classOf[UserSettingsDatabase]).readAll
      lookup(classOf[MarketDataStore]).readAll()
      val tradeStores = lookup(classOf[TradeStores])
      for (tradeStore <- tradeStores.all) {
        Log.infoWithTime("Reading all trades for " + tradeStore.tableName) {
          tradeStore.readAll()
          println("Trade Count for " + tradeStore.tableName + " = " + tradeStore.tradeCount)
        }
      }
    }
  }
}
}