package starling.services.rpc.logistics

import starling.props.Props
import com.trafigura.services.security.ComponentTestClientExecutor
import org.jboss.resteasy.client.{ProxyFactory, ClientExecutor}
import java.net.URL
import com.trafigura.edm.logistics.inventory._
import com.trafigura.tradinghub.support.ModelObject
import org.codehaus.jettison.json.{JSONObject, JSONArray}
import java.io.{BufferedWriter, FileWriter}
import starling.services.StarlingInit
import com.trafigura.edm.physicaltradespecs.EDMQuota
import com.trafigura.edm.trades.{Trade => EDMTrade, PhysicalTrade => EDMPhysicalTrade}
import scala.util.control.Exception.catching
import starling.services.rpc.refdata.{NeptuneId, TitanEdmTradeService}


/**
 * logistics service interface
 */
object LogisticsServices {
  type EdmAssignmentServiceWithGetAllAssignments = EdmAssignmentService with Object { def getAllAssignments() : List[EDMAssignmentItem] }
  type EdmInventoryServiceWithGetAllInventory = EdmInventoryService with Object { def getAllInventoryLeaves() : List[EDMInventoryItem] }
}
import LogisticsServices._
trait TitanLogisticsAssignmentServices extends ServiceProxy[EdmAssignmentServiceWithGetAllAssignments]
trait TitanLogisticsInventoryServices extends ServiceProxy[EdmInventoryServiceWithGetAllInventory]

trait ServiceProxy[T] {
  val service : T
}

trait TitanLogisticsServices {
  val assignmentService : TitanLogisticsAssignmentServices
  val inventoryService : TitanLogisticsInventoryServices
}

case class DefaultTitanLogisticsServices(props: Props) extends TitanLogisticsServices {
  val assignmentService = DefaultTitanLogisticsAssignmentServices(props)
  val inventoryService = DefaultTitanLogisticsInventoryServices(props)
}

case class DefaultTitanLogisticsAssignmentServices(props: Props) extends TitanLogisticsAssignmentServices {
  private val rmetadminuser = props.ServiceInternalAdminUser()
  private val logisticsServiceURL = props.TitanLogisticsServiceUrl()
  private lazy val clientExecutor: ClientExecutor = new ComponentTestClientExecutor(rmetadminuser)

  lazy val service: EdmAssignmentServiceWithGetAllAssignments = new EdmAssignmentServiceResourceProxy(ProxyFactory.create(classOf[EdmAssignmentServiceResource], logisticsServiceURL, clientExecutor)) {
    def getAllAssignments() : List[EDMAssignmentItem] = throw new Exception("Not implemented")
  }
}

case class DefaultTitanLogisticsInventoryServices(props: Props) extends TitanLogisticsInventoryServices {

  private val rmetadminuser = props.ServiceInternalAdminUser()
  private val logisticsServiceURL = props.TitanLogisticsServiceUrl()
  private lazy val clientExecutor: ClientExecutor = new ComponentTestClientExecutor(rmetadminuser)

  lazy val service: EdmInventoryService with Object { def getAllInventoryLeaves() : List[EDMInventoryItem] } = new EdmInventoryServiceResourceProxy(ProxyFactory.create(classOf[EdmInventoryServiceResource], logisticsServiceURL, clientExecutor)) {
    def getAllInventoryLeaves() : List[EDMInventoryItem] = throw new Exception("Not implemented")
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
    //val jsonAssignments = new JSONArray(loadJsonValuesFromFileUrl(getFileUrl(allAssignmentsFile)).mkString)
    //val allAssignments = (0 until jsonAssignments.length()).map(idx => EDMAssignmentItem.fromJson(jsonAssignments.getJSONObject(idx)).asInstanceOf[EDMAssignmentItem]).toList
  val allAssignments = jsonAssignments.map(e => EDMAssignmentItem.fromJson(new JSONObject(e)).asInstanceOf[EDMAssignmentItem])
    println("Loaded %d assignments ".format(allAssignments.size))
    allAssignments
  }

  private def readSalesAssignments() : List[EDMAssignmentItem] = {
    val salesAssignmentsFile = resourcePath + "logisticsEdmAllSalesAssignments.json"
    val jsonAssignments = new JSONArray(loadJsonValuesFromFileUrl(getFileUrl(salesAssignmentsFile)).mkString)
    val salesAssignments = (0 until jsonAssignments.length()).map(idx => EDMAssignmentItem.fromJson(jsonAssignments.getJSONObject(idx)).asInstanceOf[EDMAssignmentItem]).toList
    
    println("Loaded %d sales assignments ".format(salesAssignments.size, salesAssignments.mkString("\n")))
    salesAssignments
  }

  private val allAssignments = readAllAssignments()
  private val salesAssignments = readSalesAssignments()
}

case class FileMockedTitanLogisticsInventoryServices() extends TitanLogisticsInventoryServices {
  import FileUtils._
  // TODO: get canned mock data for inventory...
  println("starting file mocked titan logistics services")
  val inventoryFile = "/tests/valuationservice/testdata/logisticsEdmInventory.json"
  val inventoryPath = getFileUrl(inventoryFile)
  val loadedInventory = loadJsonValuesFromFileUrl(inventoryPath).map(s => EDMInventoryItem.fromJson(new JSONObject(s)).asInstanceOf[EDMInventoryItem])
  //val loadedInventory = (0 until jsonInventory.length()).map(idx => EDMInventoryItem.fromJson(jsonInventory.getJSONObject(idx)).asInstanceOf[EDMInventoryItem]).toList
  //println("Loaded %d inventory items \n %s".format(loadedInventory.size, loadedInventory.mkString("\n")))
  
//  val assignmentsFile = "/tests/valuationservice/testdata/logisticsEdmAllSalesAssignments.json"
//  val jsonAssignments = new JSONArray(loadJsonValuesFromFileUrl(getFileUrl(assignmentsFile)).mkString)
//  val loadedAssignments = (0 until jsonAssignments.length()).map(idx => EDMInventoryItem.fromJson(jsonAssignments.getJSONObject(idx)).asInstanceOf[EDMInventoryItem]).toList
//  println("Loaded %d assignments \n %s".format(loadedAssignments.size, loadedAssignments.mkString("\n")))

  lazy val service : EdmInventoryServiceWithGetAllInventory = new EdmInventoryService() {
    def getInventoryById(inventoryId : Int) : EDMInventoryItem = loadedInventory.find(i => i.oid == inventoryId).get // mimick service by throwning an exception on not found
    def getInventoryTreeByPurchaseQuotaId(quotaId : String) : List[EDMInventoryItem] = loadedInventory.filter(_.quotaName == quotaId)
    def getAllInventoryLeaves() : List[EDMInventoryItem] = findLeaves(loadedInventory)
    
    // temporary work around to find leaves of the logistics inventory tree
    private def findLeaves(inventory : List[EDMInventoryItem]) : List[EDMInventoryItem] =
      inventory.filter(item => !inventory.exists(i => i.parentId == Some(item.oid)))
  }
}

object FileUtils {
  import scala.io.Source._
  def getFileUrl(file : String) = getClass.getResource(file)
  def loadJsonValuesFromFileUrl(fileUrl : URL) : List[String] = fromURL(fileUrl).getLines.toList
  def loadJsonValuesFromFile(filePath : String) : List[String] = fromFile(filePath).getLines.toList

  def writeJson[T <: ModelObject with Object { def toJson() : JSONObject }](fileName : String, objects : List[T]) {
    try {
      val fStream = new FileWriter(fileName)
      val bWriter = new BufferedWriter(fStream)
      objects.foreach(obj => bWriter.write(obj.toJson().toString() + "\n" ))
      bWriter.flush()
      fStream.close()
    }
    catch {
      case ex : Exception => println("Error: " + ex.getMessage())
    }
  }
}

case class LogisticsJsonMockDataFileGenerater(titanEdmTradeService : TitanEdmTradeService, logisticsServices : TitanLogisticsServices) {
  import FileUtils._
  val fileOutputPath = "/tmp"
  val assignmentService = logisticsServices.assignmentService.service
  val inventoryService = logisticsServices.inventoryService.service
  val trades : List[EDMPhysicalTrade] = titanEdmTradeService.titanGetEdmTradesService.getAll().results.map(_.trade).filter(_ != null).map(_.asInstanceOf[EDMPhysicalTrade])
  val purchaseQuotas : List[EDMQuota] = trades.filter(_.direction == "P").flatMap(t => t.quotas)
  //purchaseQuotas.foreach(pq => println("pq = " + pq.detail.identifier))

  def inventoryByPurchaseQuotaId(id : String) =
    catching(classOf[Exception]) either inventoryService.getInventoryTreeByPurchaseQuotaId(id)

  /*
    val inventoryMap = purchaseQuotas.flatMap(q => {
      val inv = inventoryByPurchaseQuotaId(q.detail.identifier)
      inv.map(i => i.oid -> i)
    }).toMap
  */

  val quotaIds = purchaseQuotas.map(q => NeptuneId(q.detail.identifier).identifier).filter(id => id != null)
  println("quotaIds %d".format(quotaIds.size))
  //quotaIds.foreach(qid => println("purchase neptune quota id " + qid))
  val inventory = quotaIds.map(id => inventoryByPurchaseQuotaId(id)).collect({ case Right(i) => i }).flatten
  val inventoryLeaves = findLeaves(inventory)
  //println("Inventory, loaded %d inventory items and found %d leaves".format(inventory.size, inventoryLeaves.size))

  val inventoryFilePath = fileOutputPath + "/logisticsEdmInventory.json"
  writeJson(inventoryFilePath, inventory)

  println("getting assignments...")
  val assignments : List[EDMAssignmentItem] = inventory.flatMap(i => {
    val pa = assignmentService.getAssignmentById(i.purchaseAssignmentId) :: Nil
    i.salesAssignmentId match {
      case Some(salesId) => assignmentService.getAssignmentById(salesId) :: pa
      case None => pa
    }
  })

  println("All assignments, loaded %d assignments".format(assignments.size))
  val assignmentsFilePath = fileOutputPath + "/logisticsEdmAllAssigngments.json"
  writeJson(assignmentsFilePath, assignments)

  // temporary work around to find leaves of the logistics inventory tree
  def findLeaves(inventory : List[EDMInventoryItem]) : List[EDMInventoryItem] =
    inventory.filter(item => !inventory.exists(i => i.parentId == Some(item.oid)))
}

object LogisticServices {

  def main(args : Array[String]) {
    println("running main for Logistics services")
    val server = StarlingInit.devInstance
    val edmTradeService = server.titanServices
    val logisticsServices = server.logisticsServices
    LogisticsJsonMockDataFileGenerater(edmTradeService, logisticsServices)
    server.stop
  }
}

