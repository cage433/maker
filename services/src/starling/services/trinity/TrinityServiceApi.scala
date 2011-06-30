package starling.services.trinity

import com.trafigura.edm.shared.types.Date


case class RateCurve

case class DepoRate(bid: Double, offer: Double, period: String, date: Date)

trait TrinityServiceApi {
  def getProfileId(name: String, visibility: String): Int

  def getDepoRates(profileId: Int, commodity: String): List[DepoRate]
}

object TrinityServiceApi {
  def create(host: String, port: Int, database: String, userName: String, password: String): TrinityServiceApi = null
}