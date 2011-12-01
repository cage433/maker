package starling.manual

import starling.daterange.Day
import starling.props.PropsHelper
import starling.services.StarlingInit
import starling.curves.readers.OilAndMetalsVARLimMarketDataSource
import starling.gui.api.{PricingGroup, MarketDataSelection}
import starling.auth.AuthHandler
import starling.manager.Broadcaster

object OilAndMetalsVARLimMarketDataSourceTests {
  def main(args:Array[String]) {
    val init = new StarlingInit(PropsHelper.defaultProps, AuthHandler.Dev, Broadcaster.Null, false, false, false, false)

    //new OilAndMetalsVARLimMarketDataSource(init.limServer).read(Day(2011, 3, 31))

    init.marketDataStore.importData(MarketDataSelection(Some(PricingGroup.Metals)), Day(2011, 3, 29))
  }
}