package starling.utils

import starling.services.StarlingInit
import starling.gui.api.{PricingGroup, MarketDataSelection}
import starling.props.PropsHelper
import starling.daterange.{ObservationTimeOfDay, TimeOfDay, ObservationPoint, Day}

/**
 * Repopulates the last 100 days of market data. Handy when you add a new market and if we need
 * the historic data for VaR.
 */
object RepopulateMarketData {
  def main(args: Array[String]) {
    val init = new StarlingInit(PropsHelper.defaultProps, false, false, false, false)
    init.start
    init.marketDataStore.importData(MarketDataSelection(Some(PricingGroup.Metals)), Day(2011, 3, 31))
  }

  def foo(args: Array[String]) {
    val init = new StarlingInit(PropsHelper.defaultProps, false, false, false, false)
    init.start
    if (args.length < 1) {
      Log.error("Need a pricing group:")
      PricingGroup.values.map(p => println(p.name))
      sys.exit(-1)
    }
    PricingGroup.values.find(p => p.name.equalsIgnoreCase(args(0))) match {
      case Some(pg) => {
        val mds = MarketDataSelection(Some(pg))

        Log.info("Importing 100 days of market data for " + pg)
        val today = Day.today
        val start = today - 100
        var d = today
        while (d >= start) {
          d = d.previousBusinessDay(init.businessCalendars.UK)
          try {
            Log.info("Importing market data for " + pg + " for " + d)
            init.marketDataStore.importData(mds, d)
          }
          catch {
            case e => {
              Log.warn("problem with import", e)
            }
          }
        }
      }
      case None => throw new Exception("Didn't recognise " + args(0))
    }

  }
}
