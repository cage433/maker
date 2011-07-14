package starling.services.rpc.logistics

import starling.props.Props
import com.trafigura.services.security.ComponentTestClientExecutor
import org.jboss.resteasy.client.{ProxyFactory, ClientExecutor}
import java.net.URL
import com.trafigura.edm.logistics.inventory._
import starling.services.rpc.refdata.TitanEdmTradeService
import com.trafigura.tradinghub.support.ModelObject
import org.codehaus.jettison.json.{JSONObject, JSONArray}
import java.io.{BufferedWriter, FileWriter}
import starling.services.StarlingInit
import com.trafigura.edm.physicaltradespecs.EDMQuota
import javax.management.remote.rmi._RMIConnection_Stub
import com.trafigura.edm.trades.{Trade => EDMTrade, PhysicalTrade => EDMPhysicalTrade}

/**
 * logistics service interface
 */
trait TitanLogisticsAssignmentServices extends ServiceProxy[EdmAssignmentService]
trait TitanLogisticsInventoryServices extends ServiceProxy[EdmInventoryService]

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

  lazy val service: EdmAssignmentService = new EdmAssignmentServiceResourceProxy(ProxyFactory.create(classOf[EdmAssignmentServiceResource], logisticsServiceURL, clientExecutor))
}

case class DefaultTitanLogisticsInventoryServices(props: Props) extends TitanLogisticsInventoryServices {

  private val rmetadminuser = props.ServiceInternalAdminUser()
  private val logisticsServiceURL = props.TitanLogisticsServiceUrl()
  private lazy val clientExecutor: ClientExecutor = new ComponentTestClientExecutor(rmetadminuser)

  lazy val service: EdmInventoryService = new EdmInventoryServiceResourceProxy(ProxyFactory.create(classOf[EdmInventoryServiceResource], logisticsServiceURL, clientExecutor))
}


/**
 * service mocks...
 */
case class FileMockedTitanLogisticsServices() extends TitanLogisticsServices {
  val assignmentService = FileMockedTitanLogisticsAssignmentServices()
  val inventoryService = FileMockedTitanLogisticsInventoryServices()
}

case class FileMockedTitanLogisticsAssignmentServices() extends TitanLogisticsAssignmentServices {
  import FileUtils._
  val assignmentsFile = "/tests/valuationservice/testdata/logisticsEdmAllSalesAssignments.json"
  val jsonAssignments = new JSONArray(loadJsonValuesFromFileUrl(getFileUrl(assignmentsFile)).mkString)
  val loadedAssignments = (0 until jsonAssignments.length()).map(idx => EDMAssignmentItem.fromJson(jsonAssignments.getJSONObject(idx)).asInstanceOf[EDMAssignmentItem]).toList
  println("Loaded assignments " + loadedAssignments.mkString("\n"))
  
  lazy val service : EdmAssignmentService = new EdmAssignmentService() {
    def getAllSalesAssignments() : List[EDMAssignmentItem] = loadedAssignments
    def getAssignmentById(assignmentId : Int) : EDMAssignmentItem = null
  }
}

case class FileMockedTitanLogisticsInventoryServices() extends TitanLogisticsInventoryServices {
  import FileUtils._
  // TODO: get canned mock data for inventory...
  val assignmentsFile = "/tests/valuationservice/testdata/logisticsEdmPurchaseInventory.json"
  val jsonAssignments = new JSONArray(loadJsonValuesFromFileUrl(getFileUrl(assignmentsFile)).mkString)
  val loadedAssignments = (0 until jsonAssignments.length()).map(idx => EDMInventoryItem.fromJson(jsonAssignments.getJSONObject(idx)).asInstanceOf[EDMInventoryItem]).toList
  println("Loaded inventory " + loadedAssignments.mkString("\n"))

  lazy val service : EdmInventoryService = new EdmInventoryService() {
    def getInventoryById(inventoryId : Int) : EDMInventoryItem = null
    def getInventoryTreeByPurchaseQuotaId(quotaId : String) : List[EDMInventoryItem] = Nil
  }
}

object FileUtils {
  import scala.io.Source._
  def getFileUrl(file : String) = getClass.getResource(file)
  def loadJsonValuesFromFileUrl(fileUrl : URL) : List[String] = fromURL(fileUrl).getLines.toList

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

case class LogisticsJsonMockDataGenerater(titanEdmTradeService : TitanEdmTradeService, logisticsServices : TitanLogisticsServices) {
  import FileUtils._
  val fileOutputPath = "/tmp"
  val trades : List[EDMPhysicalTrade] = {
    val trades : List[EDMTrade] = titanEdmTradeService.titanGetEdmTradesService.getAll().results.map(_.trade).filter(_ != null)
  //  trades.foreach{t => println(t.getClass + ", " + t.tradeId)}
    trades.map(_.asInstanceOf[EDMPhysicalTrade])
  }
  val purchaseQuotas : List[EDMQuota] = trades.filter(_.direction == "P").flatMap(t => t.quotas)
  purchaseQuotas.foreach(pq => println("pq = " + pq.detail.identifier))

  def inventoryByPurchaseQuotaId(id : String) : Option[List[EDMInventoryItem]] = try {
      Some(logisticsServices.inventoryService.service.getInventoryTreeByPurchaseQuotaId(id))
    }
    catch {
      case ex : Exception => None
    }

    /*
      val inventoryMap = purchaseQuotas.flatMap(q => {
        val inv = inventoryByPurchaseQuotaId(q.detail.identifier)
        inv.map(i => i.oid -> i)
      }).toMap
    */

  val quotaIds = purchaseQuotas.map(q => NeptuneId(q.detail.identifier).identifier).filter(id => id != null)
  quotaIds.foreach(qid => println("purchase neptune quota id " + qid))
  val inventory = quotaIds.map(id => inventoryByPurchaseQuotaId(id)).collect({ case Some(i) => i }).flatten

  val inventoryFilePath = fileOutputPath + "/logisticsInventory.json"
  writeJson(inventoryFilePath, inventory)

  case class NeptuneId(id : String) {
    def identifier : String = identifier(id)
    def identifier(ident : String) : String = ident match {
      case i : String if i != null => {
        val neptunePrefix = "NEPTUNE:"
        ident.substring(neptunePrefix.length)
      }
      case null => null
    }
  }
}

object LogisticServices {

  def main(args : Array[String]) {
    val server = StarlingInit.devInstance
    val edmTradeService = server.titanServices
    val logisticsServices = server.logisticsServices
    LogisticsJsonMockDataGenerater(edmTradeService, logisticsServices)
    server.stop
  }
}
