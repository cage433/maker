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

class TestReporterActor extends Actor{

  val masterPort = System.getProperty("maker.test.manager.port").toInt
  val module = System.getProperty("maker.test.module")

  val managerPath = s"akka.tcp://TestManager@127.0.0.1:$masterPort/user/manager"
  val manager = context.actorSelection(managerPath)

  override def preStart(){
    manager ! ("Name", "Started")
  }

  def receive = {

    case event : RunCompleted =>
      sender ! Patterns.ask(manager, event, 10 * 1000)

    case event : Event =>
      manager ! event

    case other =>
      println("Debug: " + (new java.util.Date()) + " ActorTestReporter: received " + other)

  }
}

class AkkaTestReporter extends Reporter{

  private val config = {
    val text = """
      akka {
        loggers = ["akka.event.slf4j.Slf4jLogger"]
        actor {
          provider = "akka.remote.RemoteActorRefProvider"
        }
        log-sent-massages = on
        log-received-massages = on
        remote {
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

  def apply(event : Event){
    event match {
      case _ : RunCompleted =>
        val future = Patterns.ask(actor, event, 10 * 1000)
        Await.result(future, Timeout(30 * 60 * 1000).duration)
        system.shutdown
      case _ =>
        actor ! event
    }
  }
}

