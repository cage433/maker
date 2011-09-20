package starling.startserver.apps

import starling.client.MarketDataClient
import com.trafigura.services.ResteasyServiceApi


object MarketDataHttpClient {
  def main(args: Array[String]) {
    MarketDataClient.test(ResteasyServiceApi("http://localhost:37220/RPC"))
  }
}