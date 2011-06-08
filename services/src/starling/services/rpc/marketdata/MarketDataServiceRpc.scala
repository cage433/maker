package starling.services.rpc.marketdata

import com.trafigura.edm.marketdata._

class MarketDataServiceRPC()
        extends MarketDataService {

  def marketData(parameters : MarketDataRequestParameters) : MarketDataResponse = {

    // returns a mock for the moment
    new MarketDataResponse() {
      parameters = parameters
      version = 1.0
    }
  }

  //override def requireFilters(filterClasses:String*) {}
}
