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

  def apply(event : Event){
    event match {
      case _ : RunCompleted =>
        import scala.concurrent.duration._
        import akka.pattern.ask
        implicit val timeout = Timeout(10 seconds)
        val future = actor ? event
        val result = Await.result(future, timeout.duration)
        system.shutdown
      case _ =>
        actor ! event
    }
  }
}

class TestReporterReceiver extends Receiver{
  def active(remoteSystem : ActorSystem, manager : ActorRef) : PartialFunction[Any, Unit] = {
    case event : Event =>
      manager ! event
    
    case "stop" =>
      println("Shutting down")
      remoteSystem.shutdown
  }
}

class TestReporterActor extends Actor{

  val masterPort = System.getProperty("maker.test.manager.port").toInt
  val masterName = System.getProperty("maker.test.manager.name")
  val module = System.getProperty("maker.test.module")

  val managerPath = s"akka.tcp://MAKER-ACTOR-SYSTEM@127.0.0.1:$masterPort/user/$masterName"
  val managerSelection = context.actorSelection(managerPath)

  context.setReceiveTimeout(3.seconds)
  sendIdentifyRequest()

  private def sendIdentifyRequest(){
    managerSelection ! Identify(managerPath)
  }

  var runComplete = false
  var events : List[(ActorRef, Event)] = Nil

  def receive = {
    case ActorIdentity(`managerPath`, Some(manager)) =>
      context.setReceiveTimeout(Duration.Undefined)
      processEvents(manager)
      context.become(active(manager))

    case ActorIdentity(`managerPath`, None) => 
      println(s"Remote actor not availible: $managerPath")

    case ReceiveTimeout => 
      sendIdentifyRequest()

    case event : Event => 
      events = (sender, event) :: events
  }

  def processEvents(manager : ActorRef){
    events.reverse.foreach{
      case (sender, event : RunCompleted) =>
        implicit val timeout = Timeout(10 seconds)
        val future = Patterns.ask(manager, event, 10 * 1000)
        val result = Await.result(future, timeout.duration)
        sender ! result

      case (_, event) =>
        manager ! event
    }
    events = Nil
  }

  def active(manager : ActorRef) : Actor.Receive = {
    case event : Event =>
      events = (sender, event) :: events
      processEvents(manager)

    case other =>
      println("Debug: " + (new java.util.Date()) + " ActorTestReporter: unexpected event " + other)
  }
}


