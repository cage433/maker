package starling.services.rpc.logistics

import starling.props.Props
import com.trafigura.services.security.ComponentTestClientExecutor
import org.jboss.resteasy.client.{ProxyFactory, ClientExecutor}
import java.net.URL
import com.trafigura.edm.logistics.inventory._
import com.trafigura.tradinghub.support.ModelObject
import org.codehaus.jettison.json.JSONObject
import starling.services.StarlingInit
import com.trafigura.edm.physicaltradespecs.EDMQuota
import com.trafigura.edm.trades.{PhysicalTrade => EDMPhysicalTrade}
import scala.util.control.Exception.catching
import starling.services.rpc.valuation.DefaultTitanTradeService
import starling.services.rpc.refdata.FileMockedTitanServices
import starling.titan.LogisticsServices._
import starling.titan._
import java.io._
import java.util.zip.{GZIPInputStream, GZIPOutputStream}


/**
 * logistics service interface
 */
case class DefaultTitanLogisticsServices(props: Props, titanEdmTradeService : Option[TitanServices] = None) extends TitanLogisticsServices {
  val inventoryService = DefaultTitanLogisticsInventoryServices(props, titanEdmTradeService)
  val assignmentService = DefaultTitanLogisticsAssignmentServices(props, Some(inventoryService))
}

case class DefaultTitanLogisticsAssignmentServices(props: Props, titanInventoryService : Option[TitanLogisticsInventoryServices]) extends TitanLogisticsAssignmentServices {
  private val rmetadminuser = props.ServiceInternalAdminUser()
  private val logisticsServiceURL = props.TitanLogisticsServiceUrl()
  private lazy val clientExecutor: ClientExecutor = new ComponentTestClientExecutor(rmetadminuser)

  lazy val service: EdmAssignmentServiceWithGetAllAssignments = new EdmAssignmentServiceResourceProxy(ProxyFactory.create(classOf[EdmAssignmentServiceResource], logisticsServiceURL, clientExecutor)) {
    def getAllAssignments() : List[EDMAssignmentItem] = {
      titanInventoryService match {
        case Some(inventoryService) => {
          val inventory = inventoryService.service.getAllInventoryLeaves()
          val assignments : List[EDMAssignmentItem] = inventory.flatMap(i => {
          val pa = this.getAssignmentById(i.purchaseAssignmentId) :: Nil
            i.salesAssignmentId match {
              case Some(salesId) => this.getAssignmentById(salesId) :: pa
              case None => pa
            }
          })
          assignments
        }
        case _ => throw new UnsupportedOperationException
      }
    }
  }
}

case class DefaultTitanLogisticsInventoryServices(props: Props, titanEdmTradeService : Option[TitanServices] = None) extends TitanLogisticsInventoryServices {

  private val rmetadminuser = props.ServiceInternalAdminUser()
  private val logisticsServiceURL = props.TitanLogisticsServiceUrl()
  private lazy val clientExecutor: ClientExecutor = new ComponentTestClientExecutor(rmetadminuser)

  lazy val service: EdmInventoryService with Object { def getAllInventoryLeaves() : List[EDMInventoryItem] } =
    new EdmInventoryServiceResourceProxy(ProxyFactory.create(classOf[EdmInventoryServiceResource], logisticsServiceURL, clientExecutor)) {
      def getAllInventoryLeaves() : List[EDMInventoryItem] = {

        titanEdmTradeService match {
          case Some(edmTradeService) => {
            /**
             * temporary faked logisitcs service to get all inventory (by fetching all inventory by purchase quota ids)
             */
            val trades : List[EDMPhysicalTrade] = edmTradeService.titanGetEdmTradesService.getAll().results.map(_.trade).filter(_ != null).map(_.asInstanceOf[EDMPhysicalTrade])
            val purchaseQuotas : List[EDMQuota] = trades.filter(_.direction == "P").flatMap(t => t.quotas)
            def inventoryByPurchaseQuotaId(id : String) =
              catching(classOf[Exception]) either this.getInventoryTreeByPurchaseQuotaId(id)

            val quotaIds = purchaseQuotas.map(q => NeptuneId(q.detail.identifier).identifier).filter(_ != null)
            val allInventory = quotaIds.map(id => inventoryByPurchaseQuotaId(id))
            val inventory = allInventory.collect({ case Right(i) => i }).flatten
            println("Fetched all inventory (%d)".format(inventory.size))

            def findLeaves(inventory : List[EDMInventoryItem]) : List[EDMInventoryItem] =
              inventory.filter(item => !inventory.exists(i => i.parentId == Some(item.oid)))

            findLeaves(inventory)
          }
          case _ => throw new UnsupportedOperationException
        }
      }
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
    //println("Loaded %d assignments ".format(allAssignments.size))
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
  // TODO: get canned mock data for inventory...
  println("starting file mocked titan logistics services")
  val inventoryFile = "/tests/valuationservice/testdata/logisticsEdmInventory.json"
  val inventoryPath = getFileUrl(inventoryFile)
  val loadedInventory = loadJsonValuesFromFileUrl(inventoryPath).map(s => EDMInventoryItem.fromJson(new JSONObject(s)).asInstanceOf[EDMInventoryItem])
  var inventoryMap : Map[String, EDMInventoryItem] = loadedInventory.map(i => i.oid.contents.toString -> i).toMap

  //val loadedInventory = (0 until jsonInventory.length()).map(idx => EDMInventoryItem.fromJson(jsonInventory.getJSONObject(idx)).asInstanceOf[EDMInventoryItem]).toList
  //println("Loaded %d inventory items \n %s".format(loadedInventory.size, loadedInventory.mkString("\n")))
  
//  val assignmentsFile = "/tests/valuationservice/testdata/logisticsEdmAllSalesAssignments.json"
//  val jsonAssignments = new JSONArray(loadJsonValuesFromFileUrl(getFileUrl(assignmentsFile)).mkString)
//  val loadedAssignments = (0 until jsonAssignments.length()).map(idx => EDMInventoryItem.fromJson(jsonAssignments.getJSONObject(idx)).asInstanceOf[EDMInventoryItem]).toList
//  println("Loaded %d assignments \n %s".format(loadedAssignments.size, loadedAssignments.mkString("\n")))

  lazy val service : EdmInventoryServiceWithGetAllInventory = new EdmInventoryService() {
    def getInventoryById(inventoryId : Int) : EDMInventoryItem = inventoryMap(inventoryId.toString) // mimick service by throwning an exception on not found
    def getInventoryTreeByPurchaseQuotaId(quotaId : String) : List[EDMInventoryItem] = inventoryMap.values.toList.filter(_.quotaName == quotaId)
    def getAllInventoryLeaves() : List[EDMInventoryItem] = findLeaves(inventoryMap.values.toList)
    
    // temporary work around to find leaves of the logistics inventory tree
    private def findLeaves(inventory : List[EDMInventoryItem]) : List[EDMInventoryItem] =
      inventory.filter(item => !inventory.exists(i => i.parentId == Some(item.oid)))
  }

  def updateInventory(item : EDMInventoryItem) {
    inventoryMap = inventoryMap.updated(item.oid.contents.toString, item)
  }
}

object FileUtils {
  import scala.io.Source._
  def getFileUrl(file : String) = getClass.getResource(file)

  def loadJsonValuesFromFileUrl(fileUrl : URL, compress : Boolean = false) : List[String] = {
    //println("load json from file url " + fileUrl)
    val bufferSize = 512 * 1024
    if (!compress) {
      fromURL(fileUrl).getLines.toList
    }
    else {
      try {
        val fStream = new FileInputStream(new File(fileUrl.toURI))
        var inStream = if (compress) {
          new BufferedInputStream(new GZIPInputStream(fStream), bufferSize)
        }
        else {
          new BufferedInputStream(fStream)
        }

        var data = ""
        var bytesRead = 0
        val dataBuffer = Array.ofDim[Byte](bufferSize)
        while ({ bytesRead = inStream.read(dataBuffer); bytesRead != -1 }) {
          //println("read %d bytes".format(bytesRead))
          if (bytesRead > 0) {
            val dataChunk = new String(dataBuffer, 0, bytesRead)
            data += dataChunk
          }
        }
        val lines : List[String] = data.split("\n").toList
        lines
      }
      catch {
        case ex : Exception => println("Error: " + ex.getMessage()); throw ex
      }
    }
  }

  def loadJsonValuesFromFile(filePath : String, compress : Boolean = false) : List[String] =
    loadJsonValuesFromFileUrl(new File(filePath).toURI.toURL, compress)

  def writeJson[T <: ModelObject with Object { def toJson() : JSONObject }](fileName : String, objects : List[T], compress : Boolean = false) {

    try {
      val fStream = new FileOutputStream(fileName)
      var outStream = if (compress) {
        new BufferedOutputStream(new GZIPOutputStream(fStream))
      }
      else {
        new BufferedOutputStream(fStream)
      }

      objects.foreach(obj => outStream.write((obj.toJson() + "\n").getBytes()))
      outStream.flush()
      outStream.close()
    }
    catch {
      case ex : Exception => println("Error: " + ex.getMessage())
    }
  }
}

case class LogisticsJsonMockDataFileGenerater(titanEdmTradeService : TitanServices, logisticsServices : TitanLogisticsServices) {
  import FileUtils._
  val fileOutputPath = "/tmp"
  val inventoryFilePath = fileOutputPath + "/logisticsEdmInventory.json"
  val assignmentsFilePath = fileOutputPath + "/logisticsEdmAllAssignments.json"
  val salesAssignmentsFilePath = fileOutputPath + "/logisticsEdmAllSalesAssignments.json"

  val assignmentService = logisticsServices.assignmentService.service
  val inventoryService = logisticsServices.inventoryService.service
  val trades : List[EDMPhysicalTrade] = titanEdmTradeService.titanGetEdmTradesService.getAll().results.map(_.trade).filter(_ != null).map(_.asInstanceOf[EDMPhysicalTrade])
  val purchaseQuotas : List[EDMQuota] = trades.filter(_.direction == "P").flatMap(t => t.quotas)
  //purchaseQuotas.foreach(pq => println("pq = " + pq.detail.identifier))
  val allQuotasMap = trades.flatMap(t => t.quotas.map(q => NeptuneId(q.detail.identifier).identifier -> q))

  val salesAssignments = assignmentService.getAllSalesAssignments()
  println("Got %d sales assignments".format(salesAssignments.size))
  def inventoryByPurchaseQuotaId(id : String) =
    catching(classOf[Exception]) either inventoryService.getInventoryTreeByPurchaseQuotaId(id)

  val quotaIds = purchaseQuotas.map(q => NeptuneId(q.detail.identifier).identifier).filter(id => id != null)
  println("quotaIds count %d".format(quotaIds.size))
  //quotaIds.foreach(qid => println("purchase neptune quota id " + qid))
  val allInventory = quotaIds.map(id => inventoryByPurchaseQuotaId(id))
  val inventory = allInventory.collect({ case Right(i) => i }).flatten
  val invalidInventory = allInventory.collect({ case Left(i) => i })
  val inventoryLeaves = findLeaves(inventory)
  //println("Inventory, loaded %d inventory items and found %d leaves".format(inventory.size, inventoryLeaves.size))

  println("getting assignments...")
  val assignments : List[EDMAssignmentItem] = inventory.flatMap(i => {
    val pa = assignmentService.getAssignmentById(i.purchaseAssignmentId) :: Nil
    i.salesAssignmentId match {
      case Some(salesId) => assignmentService.getAssignmentById(salesId) :: pa
      case None => pa
    }
  })

  val assignmentMap = assignments.map(a => a.oid.contents -> a).toMap

  val validInventory = inventory.filter(i => {
    allQuotasMap.contains(NeptuneId(assignmentMap(i.purchaseAssignmentId).quotaName).identifier) && {
      i.salesAssignmentId match {
        case Some(salesId) => allQuotasMap.contains(NeptuneId(assignmentMap(salesId).quotaName).identifier)
        case None => true
      }
    }
  })

  writeJson(inventoryFilePath, inventory)

  println("All assignments, loaded %d assignments".format(assignments.size))
  writeJson(assignmentsFilePath, assignments)

  writeJson(salesAssignmentsFilePath, salesAssignments)

  // temporary work around to find leaves of the logistics inventory tree
  def findLeaves(inventory : List[EDMInventoryItem]) : List[EDMInventoryItem] =
    inventory.filter(item => !inventory.exists(i => i.parentId == Some(item.oid)))
}

object LogisticServices {

  def main(args : Array[String]) {
    println("running main for Logistics services")
    val server = StarlingInit.devInstance
    val edmTradeService = server.titanServices
    val mockTitanServices = new FileMockedTitanServices()
    val mockTitanTradeService = new DefaultTitanTradeService(mockTitanServices)
    val logisticsServices = server.logisticsServices
    LogisticsJsonMockDataFileGenerater(mockTitanServices, logisticsServices)
    server.stop
  }
}
