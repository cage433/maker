package starling.client


import starling.bouncyrmi.{BouncyRMIClient}
import starling.api.utils.PropertiesMapBuilder
import starling.auth.Client
import com.trafigura.services.{DisconnectableService, ServiceApi}

case class BouncyRMIServiceApi(hostnameOption: Option[String] = PropertiesMapBuilder.property("ExternalHostname"),
                               portOption: Option[String] = PropertiesMapBuilder.property("StarlingServiceRmiPort")) extends ServiceApi {

  val (hostname, port) = (hostnameOption, portOption) match {
    case (Some(h), Some(p)) => (h, p)
    case _ => throw new Exception("""
Starling service RMI hostname and port must be set. In trafigura.properties, set
          StarlingExternalHostname=<hostname>
          StarlingServiceRmiPort=<port>
        or
          trafigura.service.starling.internal=rmi://<hostname>/<port>/

        otherwise in props.conf set
          ExternalHostname= <hostname>
          StarlingServiceRmiPort= <port>

        The host and port can be found in the Starling server logs. Look for a line like

        'Initialize public services for Titan components, service host/port: <host>/<port>'
      """)
  }

  override def disconnectable[T: ClassManifest]: DisconnectableService[T] = {
    val klass = classManifest[T].erasure.asInstanceOf[Class[T]]
    val bouncyRMI = new BouncyRMIClient(hostname, port.toInt, Client.Null)

    try {
      bouncyRMI.startBlocking
    } catch {
      case e => {
        bouncyRMI.stop
        throw e
      }
    }

    new DisconnectableService[T]() {
      def disconnect {bouncyRMI.stop}
      def service = bouncyRMI.proxy(klass)
    }
  }
}
