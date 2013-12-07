package maker.scalatest

import org.scalatest.Reporter
import com.typesafe.config.ConfigFactory
import akka.actor.ActorSystem
import akka.util.Timeout
import akka.actor.Actor
import org.scalatest.events.Event
import akka.actor.Props
import akka.actor.ActorSelection
import org.scalatest.events.RunCompleted
import akka.pattern.Patterns
import scala.concurrent.Await
import scala.concurrent.duration._
import akka.actor.ActorRef
import akka.actor.Identify
import akka.actor.ActorIdentity
import akka.actor.ReceiveTimeout
import maker.akka.Receiver
import maker.akka.RemoteActor
import scala.concurrent.duration._
import akka.pattern.ask

class AkkaTestReporter extends Reporter{

  private val config = {
    val text = """
      akka {
        loggers = ["akka.event.slf4j.Slf4jLogger"]
        actor {
          provider = "akka.remote.RemoteActorRefProvider"
        }
        loglevel = off
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

  val module = System.getProperty("maker.test.module")
  val system = ActorSystem.create("REMOTE-" + module, config)

  val actor = system.actorOf(Props[TestReporterActor], "test-reporter")

  def blockOnRemoteActorAck(rc : RunCompleted){
    implicit val timeout = Timeout(10 seconds)
    val future = actor ? rc
    Await.result(future, timeout.duration)
  }

  def apply(event : Event){
    event match {
      case rc : RunCompleted =>
        blockOnRemoteActorAck(rc)
        system.shutdown
      case _ =>
        actor ! event
    }
  }
}

class TestReporterActor extends RemoteActor{

  var runComplete = false

  def activate(manager : ActorRef) = {
    def processEvents{
      def blockOnResponseFromTestManager(rc : RunCompleted) = {
        implicit val timeout = Timeout(10 seconds)
        val future = Patterns.ask(manager, rc, 10 * 1000)
        Await.result(future, timeout.duration)
      }
      
      toProcess.reverse.foreach{
        case (sender, event : RunCompleted) =>
          val response = blockOnResponseFromTestManager(event)
          sender ! response

        case (_, event) =>
          manager ! event
      }
      toProcess = Nil
    }
    processEvents
    val foo : PartialFunction[Any, Unit] = {
      case event : Event =>
        toProcess = (sender, event) :: toProcess
        processEvents
      case other =>
        println("Debug: " + (new java.util.Date()) + " ActorTestReporter: unexpected event " + other)
    }
    foo
  }
}


