package maker.akka

import org.scalatest.FunSuite
import akka.testkit.TestKitBase
import akka.actor.ActorSystem
import com.typesafe.config.ConfigFactory
import akka.actor.ExtendedActorSystem


class RemoteActorManagerTests extends FunSuite with TestKitBase{
  private val systemConfig = {
    val text = """
      akka {
        loggers = ["akka.event.slf4j.Slf4jLogger"]
        actor {
          provider = "akka.remote.RemoteActorRefProvider"
        }
        loglevel=off
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
  implicit val system = ActorSystem.create("MAKER-TEST-SYSTEM", systemConfig).asInstanceOf[ExtendedActorSystem]
  test("Remote actor call"){
  }
}
