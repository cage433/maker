package maker.akka
import akka.actor.ActorSystem
import java.util.concurrent.atomic.AtomicReference
import com.typesafe.config.ConfigFactory
import akka.actor.ExtendedActorSystem
import java.util.concurrent.atomic.AtomicInteger

/**
  * As much as I dislike this use of a global, I couldn't find a way 
  * around it. Threading through an actor system created dynamically
  * would have been difficult, given that builds create their graphs
  * at the time of execution - not least as the may not know their
  * graphs till that time.
  * In the end - given I can't think of a compelling reason to need
  * more than one actor system per JVM - I just went for a global.
  */

object MakerActorSystem{

  private val maybeSystem : AtomicReference[Option[ExtendedActorSystem]] = new AtomicReference(None)

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

  def system = synchronized{
    maybeSystem.get match {
      case Some(system) => system
      case None => {
        val system = ActorSystem.create("MAKER-ACTOR-SYSTEM", systemConfig).asInstanceOf[ExtendedActorSystem]
        maybeSystem.set(Some(system))
        system
      }
    }
  }

  def shutdown = synchronized{
    maybeSystem.get match {
      case None => 
        // Called in exec mode after each build - so may never have created a system
      case Some(system) =>
        system.shutdown
        maybeSystem.set(None)
    }
  }

  def port = synchronized{
    system.provider.getDefaultAddress.port.get
  }

  val nextActorID_ = new AtomicInteger(0)
  def nextActorID() : Int = nextActorID_.getAndIncrement

}

