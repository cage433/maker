package starling.bouncyrmi

import starling.auth.Client
import starling.auth.internal.{RealClient, ClientLogin}
import javax.security.auth.login.LoginException
import swing.Publisher
import starling.utils.{Log}
import starling.manager.{Broadcaster, BromptonContext, BromptonActivator}

case class GuiLaunchParameters(serverRmiHost:String, serverRmiPort:Int, principalName:String, runAs:Option[String])


class BouncyRMIClientBromptonActivator extends BromptonActivator with Log {
  def start(context: BromptonContext) = {
    val broadcaster = context.awaitService(classOf[Broadcaster])
    val guiLaunchParameters = context.awaitService(classOf[GuiLaunchParameters])
    val overriddenUser = guiLaunchParameters.runAs
    Log.infoWithTime("Bouncy client") {
      val client = new BouncyRMIClient(
        guiLaunchParameters.serverRmiHost,
        guiLaunchParameters.serverRmiPort,
        auth(guiLaunchParameters.principalName),
        overriddenUser = overriddenUser,
        broadcaster = broadcaster
      )
      Log.infoWithTime("Bouncy rmi connect") { client.startBlocking }

      context.registerService(classOf[MethodLogService], client.methodLogService)

      val serviceListing = client.proxy(classOf[ServicesListing])
      serviceListing.services.foreach { klass:Class[_] => {
        val service = client.proxy(klass)
        context.registerService(klass.asInstanceOf[Class[Any]], service.asInstanceOf[Any])
      }}
      context.onStopped { client.stop }
    }

  }

  def auth(servicePrincipalName: String): Client = {
    try {
      val subject = new ClientLogin().login
      new RealClient(servicePrincipalName, subject)
    } catch {
      case l: LoginException => {
        import starling.utils.Utils._
        os match {
          case Linux => {
            log.error("Failed to initialise kerberos, either it isn't used on this system or the ticket cache is stale (try krenew). Skipping kerberos.")
            Client.Null
          }
          case _: Windows => {
            throw new Exception("Windows: Failed to initialise kerberos for Starling log in.", l)
          }
          case u: UnknownOS => {
            throw new Exception(u + ": Failed to initialise kerberos for Starling log in.", l)
          }
        }
      }
    }
  }

}