package starling.startserver.apps

import com.trafigura.services.ResteasyServiceApi


object MarketDataHttpClient {
  def main(args: Array[String]) {
    MarketDataClient.test(ResteasyServiceApi("http://localhost:37214/RPC"))
  }
}