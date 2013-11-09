package maker.akka


import akka.actor._
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


  case class MakeActorManager(module : Module)  
  case class AskManagers(msg : Any)  

  case class Supervisor extends Actor{
    import context.dispatcher
    import akka.pattern.ask
    var managers : Map[Module, ActorRef] = Map.empty

    private def manager(module : Module) : ActorRef = {
      managers.get(module) match {
        case None =>
          val manager = context.actorOf(Props[AkkaTestManager.Manager], module.name) 
          context.watch(manager)
          managers += (module -> manager)
          manager
        case Some(m) => m
      }
    }

    def processRequest(sender : ActorRef, msg : Any) = msg match {

      case MakeActorManager(module) => 
        val manager = context.actorOf(Props[AkkaTestManager.Manager], module.name) 
        context.watch(manager)
        managers += (module -> manager)

      case Terminated(manager) =>
        managers = managers.filterNot(_ == manager)

      case AskManagers(msg : Any) =>
        implicit val timeout = Timeout(5 seconds)
        val future = Future.sequence(managers.values.map{m => m ? msg})

    }

    def receive = {
      case msg : Any =>
        processRequest(sender, msg)
    }
  }

  lazy val supervisor = system.actorOf(Props[Supervisor], "Supervisor")

  private def askSupervisor[T](msg : AnyRef) : T = {
    val fut = Patterns.ask(supervisor, msg, 10 * 1000)
    Await.result(fut, Duration(100, TimeUnit.SECONDS)).asInstanceOf[T]
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

