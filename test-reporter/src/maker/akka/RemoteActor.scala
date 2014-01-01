package maker.akka

import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory
import akka.actor.ActorRef
import akka.actor.ExtendedActorSystem
import akka.actor.ActorSystem
import akka.actor.Identify
import akka.actor.ActorIdentity
import akka.actor.ReceiveTimeout
import akka.actor.Actor
import scala.concurrent.Await
import scala.concurrent.duration._
import akka.actor.ActorSelection
import akka.actor.{Props => AkkaProps}
import akka.actor.Address
import com.typesafe.config.ConfigFactory

object RemoteActor {

  val localSystemAddressLabel = "maker.local.system.address"

  def javaOpts(localActor : ActorRef, localSystem : ExtendedActorSystem, remoteActorClassname : String) = {
    val localSystemAddress = localSystem.provider.getDefaultAddress.toString

    List(
      s"-D$localSystemAddressLabel=$localSystemAddress"
    )
  }

  val systemConfig = {
    val text = """
      akka {
        loggers = ["akka.event.slf4j.Slf4jLogger"]
        loglevel = "ERROR"
        log-dead-letters = off
        actor {
          provider = "akka.remote.RemoteActorRefProvider"
        }
        remote {
          log-remote-lifecycle-events = off
          enabled-transports = ["akka.remote.netty.tcp"]
          netty.tcp {
            hostname = "127.0.0.1"
            port = 0
          }
        }
      }
    """
    ConfigFactory.parseString(text)
  }

}

