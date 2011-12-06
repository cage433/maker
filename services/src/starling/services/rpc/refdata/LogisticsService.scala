package starling.services.rpc.refdata

import starling.utils.Log
import com.trafigura.services.Logistics.LogisticsServiceApi
import starling.titan.{TitanLogisticsServices, TitanServices}

class LogisticsService(titanServices: TitanServices, logisticsServices : TitanLogisticsServices)
  extends LogisticsServiceApi with Log {

  def inventoryById(id: String) : List[String] = {
    val response = logisticsServices.inventoryService.service.getInventoryById(id.toInt)
    val inventory = response.associatedInventory
    val quotas = response.associatedQuota

    val json = inventory.map(_.toJson()) ::: quotas.map(_.toJson())

    import net.liftweb.json._
    import net.liftweb.json.pretty

    val r = json.map(s => pretty(render(parse(s.toString))))
    r
  }
}
