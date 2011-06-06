package starling.curves

import readers.FwdCurveDbMarketDataSource
import starling.daterange.Day
import starling.utils.sql.Query
import starling.utils.sql.QueryBuilder._
import starling.db._
import starling.gui.api.MarketDataSelection
import starling.utils.{Log, RepeatingTask}
import starling.calendar.BusinessCalendar

class FwdCurveAutoImport(runEvery: Int, marketDataStore: MarketDataStore, marketDataSources: Set[MarketDataSet], businessCalendar: BusinessCalendar)
  extends RepeatingTask(runEvery, "EAIAutoImport") {

  def task = {
    try {
      runTask
    } catch {
      case e => Log.warn("Problem doing auto import for FC App, will try again later.", e)
    }
  }

  private def importDays = {
    // we want to try to import for the last day someone entered prices. that gets a little fuzzy when
    // bank holidays are around.
    // The logic below, for example, would mean that on the tuesday after a bank holiday Friday and Monday
    // we would try to import for Thursday, Friday and Monday. Which is more correct than just trying to import
    // for Thursday or Monday.s
    val today = Day.today
    val start = today.previousBusinessDay(businessCalendar)
    val end = today.previousWeekday
    (start upto end).toList.filter(_.isWeekday)
  }

  private def yieldMDS = {
    // a small sleep that will allow anyone else waiting on the market data store
    // to do their import in between ours
    Thread.sleep(1000)
  }

  private def runTask {
    val days = importDays
    days.map {
      importDay => {

        // import for each mds
        marketDataSources.map {
          mds => {
            try {
              marketDataStore.importFor(importDay, mds)
            } catch {
              case m: NoMarketDataForDayException =>
            }
            yieldMDS
          }
        }

        // then import for lim
        try {
          marketDataStore.importFor(importDay, MarketDataSet.LIM)
        } catch {
          case m: NoMarketDataForDayException =>
        }
        yieldMDS
      }
    }
  }
}