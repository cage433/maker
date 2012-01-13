package starling.services.rpc.refdata

import starling.utils.Log
import com.trafigura.services.Logistics.LogisticsServiceApi
import starling.titan.{TitanLogisticsServices, TitanTradeMgmtServices}
import net.liftweb.json._
import starling.webservice.{EDMFormats, JsonSerializer}

class LogisticsService(logisticsServices : TitanLogisticsServices)
  extends LogisticsServiceApi with Log {

  def inventoryById(id: String) : List[JValue] = {
    val response = logisticsServices.getInventoryById(id.toInt)
    val inventory = response.associatedInventory
    val quotas = response.associatedQuota

    val json = inventory.map(_.toJson()) ::: quotas.map(_.toJson())

    val r = json.map(s => parse(s.toString))
    r
  }
}
