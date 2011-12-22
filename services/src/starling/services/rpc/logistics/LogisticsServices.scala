package starling.services.rpc.logistics

import com.trafigura.services.security.ComponentTestClientExecutor
import org.jboss.resteasy.client.{ProxyFactory, ClientExecutor}
import com.trafigura.edm.logistics.inventory._
import com.trafigura.edm.trademgmt.physicaltradespecs.PhysicalTradeQuota
import com.trafigura.edm.trademgmt.trades.{PhysicalTrade => EDMPhysicalTrade}
import starling.services.rpc.refdata.FileMockedTitanServices
import starling.titan.LogisticsServices._
import starling.titan._
import starling.services.rpc.FileUtils
import org.codehaus.jettison.json.JSONObject
import starling.props.{PropsHelper, Props}
import com.trafigura.edm.common.units.TitanId


/**
 * logistics service interface
 */
case class DefaultTitanLogisticsServices(props: Props) extends TitanLogisticsServices {
  val inventoryService = DefaultTitanLogisticsInventoryServices(props)
}

case class DefaultTitanLogisticsInventoryServices(props: Props) extends TitanLogisticsInventoryServices {

  import JMXEnabler._
  private val rmetadminuser = props.ServiceInternalAdminUser()
  private val logisticsServiceURL = props.TitanLogisticsServiceUrl()
  private lazy val clientExecutor: ClientExecutor = new ComponentTestClientExecutor(rmetadminuser)

  lazy val service : EdmInventoryServiceWithGetAllInventory =
    new EdmInventoryServiceResourceProxy(ProxyFactory.create(classOf[EdmInventoryServiceResource], logisticsServiceURL, clientExecutor)) {

      def getAllInventory() : LogisticsInventoryResponse = {
        enableLogisticsAPIs(props) // temporary measure until we address this properly
        getInventoryByGroupCompanyId("*")
      }
    }
}


/**
 * service mocks...
 */
case class FileMockedTitanLogisticsServices() extends TitanLogisticsServices {
  val inventoryService = FileMockedTitanLogisticsInventoryServices()
}

case class FileMockedTitanLogisticsInventoryServices() extends TitanLogisticsInventoryServices {
  import FileUtils._
  import LogisticServices._
  println("starting file mocked titan logistics services")
  private val inventoryFile = "/tests/valuationservice/testdata/logisticsEdmInventory.json"
  private val inventoryPath = getFileUrl(inventoryFile)
  private val loadedInventory = loadJsonValuesFromFileUrl(inventoryPath).map(s => LogisticsInventoryResponse.fromJson(new JSONObject(s)).asInstanceOf[LogisticsInventoryResponse]).head
  
  private var inventoryMap : Map[String, InventoryItem] = loadedInventory.associatedInventory.map(i => i.oid.contents.toString -> i).toMap
  private val quotaMap : Map[String, LogisticsQuota] = loadedInventory.associatedQuota.map(q => q.quotaName -> q).toMap

  lazy val service : EdmInventoryServiceWithGetAllInventory = new EdmInventoryService() {
    def getInventoryById(inventoryId : Int) : LogisticsInventoryResponse = {
      val i = inventoryMap(inventoryId.toString) // mimick service by throwing an exception on not found
      LogisticsInventoryResponse(
        associatedQuota = List(quotaMap(i.purchaseAssignment.quotaName)) ::: Option(i.salesAssignment).map(sa => quotaMap(sa.quotaName)).toList,
        associatedInventory = List(i)
      )
    }

    def getInventoryTreeByPurchaseQuotaId(quotaId : String) : LogisticsInventoryResponse = null //inventoryMap.values.toList.filter(_.purchaseAssignment.quotaName == quotaId)
    def getInventoryByGroupCompanyId(groupCompanyMappingCode : String) = loadedInventory
    def getAllInventory() : LogisticsInventoryResponse = getInventoryByGroupCompanyId("")
    def getAllInventoryItems() : List[InventoryItem] = inventoryMap.values.toList
    def getInventoryTreeByPurchaseQuotaTitanId(id : TitanId) : LogisticsInventoryResponse = null
  }

  def updateInventory(item : InventoryItem) {
    inventoryMap = inventoryMap.updated(item.oid.contents.toString, item)
  }
}


/**
 * generates mock service data for logistics mock services
 */
case class LogisticsJsonMockDataFileGenerator(titanEdmTradeService : TitanServices, logisticsServices : TitanLogisticsServices) {
  import FileUtils._
  val fileOutputPath = "/tmp"
  val inventoryFilePath = fileOutputPath + "/logisticsEdmInventory.json"

  val inventoryService = logisticsServices.inventoryService.service
  val trades : List[EDMPhysicalTrade] = titanEdmTradeService.titanGetEdmTradesService.getAll().results.map(_.trade).filter(_ != null).map(_.asInstanceOf[EDMPhysicalTrade])

  val purchaseQuotas : List[PhysicalTradeQuota] = trades.filter(_.direction == "P").flatMap(t => t.quotas)
  val allQuotasMap = trades.flatMap(t => t.quotas.map(q => NeptuneId(q.detail.identifier.value).identifier -> q))

  val quotaIds = purchaseQuotas.map(q => NeptuneId(q.detail.identifier.value).identifier).filter(id => id != null)
  println("quotaIds count %d".format(quotaIds.size))

  val inventory = inventoryService.getAllInventory()
  println("Inventory, loaded %d associated inventory items and %d associated quotas".format(inventory.associatedInventory.size, inventory.associatedQuota.size))
  writeJson(inventoryFilePath, List(inventory))
}

object LogisticServices {

  def main(args : Array[String]) {
    val props = PropsHelper.defaultProps
    val mockTitanServices = new FileMockedTitanServices()
    val logisticsServices = new DefaultTitanLogisticsServices(props)
    LogisticsJsonMockDataFileGenerator(mockTitanServices, logisticsServices)
  }
}
