package starling.services.repl

import starling.daterange.{ObservationPoint, Day}
import starling.db.{MarketDataReaderMarketDataSlice, NormalMarketDataReader}
import starling.gui.api.{MarketDataSelection, PricingGroup}
import starling.curves.{MarketDataCurveObjectEnvironment, Environment}

/**
 * A place to put functions to make live from the command line easier
 */
object ReplFunctions  {

  def init() = starling.services.StarlingInit.devInstance

  lazy val devInstance = init()

  def makeEnv(pricingGroup : PricingGroup, marketDay : Day) : Environment = {
    val marketDataStore = devInstance.marketDataStore

    val marketDataSelection = MarketDataSelection(Some(pricingGroup), None)
    val marketDataID = marketDataStore.latestMarketDataIdentifier(marketDataSelection)
    val reader = new NormalMarketDataReader(marketDataStore, marketDataID)
    val marketDataSlice = new MarketDataReaderMarketDataSlice(reader, ObservationPoint(marketDay))
    Environment(MarketDataCurveObjectEnvironment(marketDay.endOfDay, marketDataSlice))
  }

  def importMarketData(pricingGroup : PricingGroup, marketDay : Day){
    val marketDataStore = devInstance.marketDataStore
    val marketDataSelection = MarketDataSelection(Some(pricingGroup), None)
    marketDataStore.importData(marketDataSelection, marketDay)
  }

}