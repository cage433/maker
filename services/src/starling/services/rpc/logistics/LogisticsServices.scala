package starling.services.rpc.logistics

import starling.props.Props
import com.trafigura.services.security.ComponentTestClientExecutor
import org.jboss.resteasy.client.{ProxyFactory, ClientExecutor}
import com.trafigura.edm.logistics.inventory.{EDMAssignmentItem, EdmAssignmentService, EdmAssignmentServiceResourceProxy, EdmAssignmentServiceResource}
import java.net.URL
import io.Source._
import com.trafigura.tradecapture.internal.refinedmetal.Metal
import org.codehaus.jettison.json.{JSONArray, JSONObject}
import javax.management.remote.rmi._RMIConnection_Stub
import scala.collection.JavaConversions._
import collection.JavaConversions

/**
 * logistics service interface
 */
trait TitanLogisticsServices extends ServiceProxy[EdmAssignmentService] {
  // service('EdmAssignmentService') {
  //   operation('GetAllSalesAssignments', :returns => list('EDMAssignmentItem')) {

  val service: EdmAssignmentService
}

trait ServiceProxy[T] {
  val service : T
}

case class DefaultTitanLogisticsServices(props: Props) extends TitanLogisticsServices {
  private val rmetadminuser = props.ServiceInternalAdminUser()
  private val logisticsServiceURL = props.TitanLogisticsServiceUrl()
  private lazy val clientExecutor: ClientExecutor = new ComponentTestClientExecutor(rmetadminuser)

  lazy val service: EdmAssignmentService = new EdmAssignmentServiceResourceProxy(ProxyFactory.create(classOf[EdmAssignmentServiceResource], logisticsServiceURL, clientExecutor))
}

case class FileMockedTitanLogisticsServices() extends TitanLogisticsServices {
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

object FileUtils {
  def getFileUrl(file : String) = getClass.getResource(file)
  def loadJsonValuesFromFileUrl(fileUrl : URL) : List[String] = fromURL(fileUrl).getLines.toList
}
