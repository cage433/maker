package maker.akka


import akka.pattern.ask
import akka.pattern.Patterns
import akka.util.Timeout
import com.typesafe.config.ConfigFactory
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.TimeUnit
import maker.project.Module
import maker.task.test.AkkaTestManager
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.concurrent.Future
import akka.actor.ExtendedActorSystem
import akka.actor.ActorSystem
import akka.actor.ActorRef
import maker.Props
import maker.build.BuildManager
import maker.build.Dependency

/**
  * As much as I dislike this use of a global, I couldn't find a way 
  * around it. Threading through an actor system created dynamically
  * would have been difficult, given that builds create their graphs
  * at the time of execution - not least as the may not know their
  * graphs till that time.
  * In the end - given I can't think of a compelling reason to need
  * more than one actor system per JVM, and Akka itself recommends one
  * ActorSytem per application - I just went for a global.
  */
object MakerActorSystem{

  private val maybeSystem : AtomicReference[Option[ExtendedActorSystem]] = new AtomicReference(None)

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

  private val nextBuildNumber = new AtomicInteger(-1)
  def buildManager(buildName : String, graph : Dependency.Graph, props : Props) = {
    val buildNumber  = nextBuildNumber.incrementAndGet
    val workers : List[ActorRef] = (1 to props.NumberOfTaskThreads()).toList.map{
      case i => 
        system.actorOf(BuildManager.Worker.props(), "Worker-" + buildNumber + "-" + i)
    }
    system.actorOf(BuildManager.props(buildName, graph, workers), "BuildManager-" + buildNumber)
  }
}

