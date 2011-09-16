package starling.services.rpc.logistics

import starling.props.Props
import com.trafigura.services.security.ComponentTestClientExecutor
import org.jboss.resteasy.client.{ProxyFactory, ClientExecutor}
import com.trafigura.edm.logistics.inventory._
import starling.services.StarlingInit
import com.trafigura.edm.physicaltradespecs.PhysicalTradeQuota
import com.trafigura.edm.trades.{PhysicalTrade => EDMPhysicalTrade}
import scala.util.control.Exception.catching
import starling.services.rpc.refdata.FileMockedTitanServices
import starling.titan.LogisticsServices._
import starling.titan._
import starling.services.rpc.FileUtils
import org.codehaus.jettison.json.JSONObject

/**
 * logistics service interface
 */
case class DefaultTitanLogisticsServices(props: Props) extends TitanLogisticsServices {
  val inventoryService = DefaultTitanLogisticsInventoryServices(props)
  val assignmentService = DefaultTitanLogisticsAssignmentServices(props, Some(inventoryService))
}

case class DefaultTitanLogisticsAssignmentServices(props: Props, titanInventoryService : Option[TitanLogisticsInventoryServices]) extends TitanLogisticsAssignmentServices {
  private val rmetadminuser = props.ServiceInternalAdminUser()
  private val logisticsServiceURL = props.TitanLogisticsServiceUrl()
  private lazy val clientExecutor: ClientExecutor = new ComponentTestClientExecutor(rmetadminuser)

  lazy val service: EdmAssignmentServiceWithGetAllAssignments = new EdmAssignmentServiceResourceProxy(ProxyFactory.create(classOf[EdmAssignmentServiceResource], logisticsServiceURL, clientExecutor)) {
    def getAllAssignments() : List[EDMAssignment] = {
      titanInventoryService match {
        case Some(inventoryService) => {
          val inventory = inventoryService.service.getAllInventoryLeaves()
          inventory.flatMap(assignmentsFromInventory)
        }
        case _ => throw new UnsupportedOperationException
      }
    }

    private def assignmentsFromInventory(item : EDMInventoryItem) : List[EDMAssignment] = {
      item.purchaseAssignment :: Option(item.salesAssignment).toList
    }

  }
}

case class DefaultTitanLogisticsInventoryServices(props: Props) extends TitanLogisticsInventoryServices {

  private val rmetadminuser = props.ServiceInternalAdminUser()
  private val logisticsServiceURL = props.TitanLogisticsServiceUrl()
  private lazy val clientExecutor: ClientExecutor = new ComponentTestClientExecutor(rmetadminuser)

  lazy val service : EdmInventoryServiceWithGetAllInventory /* EdmInventoryService with Object { def getAllInventoryLeaves() : List[EDMInventoryItem];  } */ =
    new EdmInventoryServiceResourceProxy(ProxyFactory.create(classOf[EdmInventoryServiceResource], logisticsServiceURL, clientExecutor)) {
      def getAllInventoryLeaves() : List[EDMInventoryItem] = LogisticServices.findLeaves(getAllInventory().associatedInventory)
      def getAllInventory() : LogisticsInventoryResponse = getInventoryByGroupCompanyId("*")
    }
}


/**
 * service mocks...
 */
case class FileMockedTitanLogisticsServices() extends TitanLogisticsServices {
  val assignmentService = FileMockedTitanLogisticsAssignmentServices()
  val inventoryService = FileMockedTitanLogisticsInventoryServices()
}

case class FileMockedTitanLogisticsAssignmentServices() extends TitanLogisticsAssignmentServices {
  
  lazy val service : EdmAssignmentServiceWithGetAllAssignments = new EdmAssignmentService() {
    def getAllSalesAssignments() : List[EDMAssignmentItem] = salesAssignments
    def getAssignmentById(assignmentId : Int) : EDMAssignmentItem = null
    def getAllAssignments() : List[EDMAssignmentItem] = allAssignments
  }

  import FileUtils._
  private val resourcePath = "/tests/valuationservice/testdata/"
  private def readAllAssignments() : List[EDMAssignmentItem] = {
    val allAssignmentsFile = resourcePath + "logisticsEdmAllAssignments.json"
    val jsonAssignments = loadJsonValuesFromFileUrl(getFileUrl(allAssignmentsFile))
    val allAssignments = jsonAssignments.map(e => EDMAssignmentItem.fromJson(new JSONObject(e)).asInstanceOf[EDMAssignmentItem])
    allAssignments
  }

  private def readSalesAssignments() : List[EDMAssignmentItem] = {
    val salesAssignmentsFile = resourcePath + "logisticsEdmAllSalesAssignments.json"
    val jsonAssignments = loadJsonValuesFromFileUrl(getFileUrl(salesAssignmentsFile))
    //val jsonAssignments = new JSONArray(loadJsonValuesFromFileUrl(getFileUrl(salesAssignmentsFile)).mkString)
    //val salesAssignments = (0 until jsonAssignments.length()).map(idx => EDMAssignmentItem.fromJson(jsonAssignments.getJSONObject(idx)).asInstanceOf[EDMAssignmentItem]).toList
    val salesAssignments = jsonAssignments.map(e => EDMAssignmentItem.fromJson(new JSONObject(e)).asInstanceOf[EDMAssignmentItem])
    //println("Loaded %d sales assignments ".format(salesAssignments.size, salesAssignments.mkString("\n")))
    salesAssignments
  }

  private lazy val allAssignments = readAllAssignments()
  private lazy val salesAssignments = readSalesAssignments()
}

case class FileMockedTitanLogisticsInventoryServices() extends TitanLogisticsInventoryServices {
  import FileUtils._
  import LogisticServices._
  println("starting file mocked titan logistics services")
  val inventoryFile = "/tests/valuationservice/testdata/logisticsEdmInventory.json"
  val inventoryPath = getFileUrl(inventoryFile)
  //val loadedInventory = loadJsonValuesFromFileUrl(inventoryPath).map(s => EDMInventoryItem.fromJson(new JSONObject(s)).asInstanceOf[EDMInventoryItem])
  val loadedInventory = loadJsonValuesFromFileUrl(inventoryPath).map(s => LogisticsInventoryResponse.fromJson(new JSONObject(s)).asInstanceOf[LogisticsInventoryResponse]).head
  
  var inventoryMap : Map[String, EDMInventoryItem] = loadedInventory.associatedInventory.map(i => i.oid.contents.toString -> i).toMap
  val quotaMap : Map[String, EDMLogisticsQuota] = loadedInventory.associatedQuota.map(q => q.quotaName -> q).toMap

  //val loadedInventory = (0 until jsonInventory.length()).map(idx => EDMInventoryItem.fromJson(jsonInventory.getJSONObject(idx)).asInstanceOf[EDMInventoryItem]).toList
//  val assignmentsFile = "/tests/valuationservice/testdata/logisticsEdmAllSalesAssignments.json"
//  val jsonAssignments = new JSONArray(loadJsonValuesFromFileUrl(getFileUrl(assignmentsFile)).mkString)
//  val loadedAssignments = (0 until jsonAssignments.length()).map(idx => EDMInventoryItem.fromJson(jsonAssignments.getJSONObject(idx)).asInstanceOf[EDMInventoryItem]).toList

  lazy val service : EdmInventoryServiceWithGetAllInventory = new EdmInventoryService() {
    def getInventoryById(inventoryId : Int) : LogisticsInventoryResponse = {
      val i = inventoryMap(inventoryId.toString) // mimick service by throwning an exception on not found
      LogisticsInventoryResponse(
        associatedQuota = List(quotaMap(i.purchaseAssignment.quotaName)) ::: Option(i.salesAssignment).map(sa => quotaMap(sa.quotaName)).toList,
        associatedInventory = List(i)
      )
    }

    def getInventoryTreeByPurchaseQuotaId(quotaId : String) : LogisticsInventoryResponse = null //inventoryMap.values.toList.filter(_.purchaseAssignment.quotaName == quotaId)
    def getAllInventoryLeaves() : List[EDMInventoryItem] = findLeaves(getAllInventoryItems())
    def getInventoryByGroupCompanyId(groupCompanyMappingCode : String) = loadedInventory
    def getAllInventory() : LogisticsInventoryResponse = getInventoryByGroupCompanyId("")
    def getAllInventoryItems() : List[EDMInventoryItem] = inventoryMap.values.toList
  }

  def updateInventory(item : EDMInventoryItem) {
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
  val assignmentsFilePath = fileOutputPath + "/logisticsEdmAllAssignments.json"
  val salesAssignmentsFilePath = fileOutputPath + "/logisticsEdmAllSalesAssignments.json"

  val assignmentService = logisticsServices.assignmentService.service
  val inventoryService = logisticsServices.inventoryService.service
  val trades : List[EDMPhysicalTrade] = titanEdmTradeService.titanGetEdmTradesService.getAll().results.map(_.trade).filter(_ != null).map(_.asInstanceOf[EDMPhysicalTrade])

  val purchaseQuotas : List[PhysicalTradeQuota] = trades.filter(_.direction == "P").flatMap(t => t.quotas)
  val allQuotasMap = trades.flatMap(t => t.quotas.map(q => NeptuneId(q.detail.identifier.value).identifier -> q))

  val salesAssignments = assignmentService.getAllSalesAssignments()
  println("Got %d sales assignments".format(salesAssignments.size))
  def inventoryByPurchaseQuotaId(id : String) =
    catching(classOf[Exception]) either inventoryService.getInventoryTreeByPurchaseQuotaId(id).associatedInventory

  val quotaIds = purchaseQuotas.map(q => NeptuneId(q.detail.identifier.value).identifier).filter(id => id != null)
  println("quotaIds count %d".format(quotaIds.size))

  //val allInventory = quotaIds.map(id => inventoryByPurchaseQuotaId(id))
  val inventory = inventoryService.getAllInventory()
  println("Inventory, loaded %d associated inventory items and %d associated quotas".format(inventory.associatedInventory.size, inventory.associatedQuota.size))

  //val inventory = allInventory.collect({ case Right(i) => i }).flatten
  //val invalidInventory = allInventory.collect({ case Left(i) => i })
  //val inventoryLeaves = findLeaves(inventory)
  //println("Inventory, loaded %d inventory items and found %d leaves".format(inventory.size, inventoryLeaves.size))

  println("getting assignments from inventory...")
  val allInventoryAssignments : List[EDMAssignment] = inventory.associatedInventory.flatMap(i => {
    val pa = assignmentService.getAssignmentById(i.purchaseAssignment.oid.contents) :: Nil
    i.salesAssignment match {
      case null => pa
      case salesAssignment : EDMAssignment => assignmentService.getAssignmentById(salesAssignment.oid.contents) :: pa
    }
  })

  val assignmentMap = allInventoryAssignments.map(a => a.oid.contents -> a).toMap

  val validInventory = inventory.associatedInventory.filter(i => {
    allQuotasMap.contains(NeptuneId(i.purchaseAssignment.quotaName).identifier) && {
      i.salesAssignment match {
        case null => true
        case salesAssignment : EDMAssignment => allQuotasMap.contains(NeptuneId(salesAssignment.quotaName).identifier)
      }
    }
  })

  writeJson(inventoryFilePath, List(inventory))

  println("All assignments, loaded %d assignments".format(allInventoryAssignments.size))
  writeJson(assignmentsFilePath, allInventoryAssignments)

  writeJson(salesAssignmentsFilePath, salesAssignments)
}

object LogisticServices {

  // temporary work around to find leaves of the logistics inventory tree from all iventory (some maybe intermediate inventory)
  def findLeaves(inventory : List[EDMInventoryItem]) : List[EDMInventoryItem] =
    inventory.filter(item => !inventory.exists(i => i.parentId == Some(item.oid)))

  def main(args : Array[String]) {
    println("running main for Logistics services")
    val server = StarlingInit.devInstance
    val mockTitanServices = new FileMockedTitanServices()
    val logisticsServices = server.logisticsServices
    LogisticsJsonMockDataFileGenerator(mockTitanServices, logisticsServices)
    server.stop
  }
}
