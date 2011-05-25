package starling.utils

import starling.services.StarlingInit
import starling.props.PropsHelper

class ReadAll(starlingInit:StarlingInit) {

  def go() {
    readUserSettings()
    readMarketData()
    readTrades()
  }

  def readUserSettings() {
    starlingInit.userSettingsDatabase.readAll
  }

  def readTrades() {
    for (tradeStore <- starlingInit.tradeStores.all) {
      Log.infoWithTime("Reading all trades for " + tradeStore.tableName) {
        tradeStore.readAll()
        println("Trade Count for " + tradeStore.tableName + " = " + tradeStore.tradeCount)
      }
    }
  }

  def readMarketData() {
    starlingInit.marketDataStore.readAll()
  }

}


object ReadAll {

  def main(args : Array[String]) {
    val starlingInit = new StarlingInit(PropsHelper.defaultProps, startRMI = false, startHttp = false, startXLLoop = false,
      startEAIAutoImportThread = false)
    val readAll = new ReadAll(starlingInit)
    readAll.go()
  }
}