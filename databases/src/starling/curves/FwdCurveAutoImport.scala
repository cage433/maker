package starling.curves

import readers.FwdCurveDbMarketDataSource
import starling.daterange.Day
import starling.utils.sql.Query
import starling.utils.sql.QueryBuilder._
import starling.db._
import starling.gui.api.MarketDataSelection
import starling.utils.{Log, RepeatingTask}

class FwdCurveAutoImport(runEvery: Int, marketDataStore: MarketDataStore, marketDataSources: Map[MarketDataSet, FwdCurveDbMarketDataSource], varSqlDB: DB)
  extends RepeatingTask(runEvery, "EAIAutoImport") {

  private def importDay = Day.today.previousWeekday

  private var currentDay = Day(2000, 1, 1)
  private var imported: Map[MarketDataSet, Boolean] = Map()

  def task = {
    if (importDay > currentDay) {
      currentDay = importDay
      imported = marketDataSources.map {
        case (mds, _) => {
          val alreadyImported = marketDataStore.latestObservationDayForMarketDataSet(mds) match {
            case Some(day) => {
              day >= currentDay
            }
            case None => false
          }
          (mds -> alreadyImported)
        }
      }
    }

    marketDataSources.map {
      case (mds, fwdc) if !imported(mds) => {
        val pricingGroupID = fwdc.pricingGroupID
        import FwdCurveDbMarketDataSource._

        if (anyPrices(varSqlDB, MetalsPriceTable, currentDay, pricingGroupID) || anyPrices(varSqlDB, NonMetalsPriceTable, currentDay, pricingGroupID)) {
          Thread.sleep(30 * 1000) // Sleep for 30 seconds first to make sure FCA has put all the prices in
          Log.info("FCAutoImport: Found market data for " + (mds, currentDay) + ", trying import.")
          imported += mds -> true
          marketDataStore.importFor(currentDay, mds, MarketDataSet.LIM)
        }
      }
      case _ =>
    }
  }
}