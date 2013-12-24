package maker.scalatest

import org.scalatest.Reporter
import com.typesafe.config.ConfigFactory
import akka.actor.ActorSystem
import akka.util.Timeout
import akka.actor.Actor
import org.scalatest.events._
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
import maker.akka.RemoteActor
import scala.concurrent.duration._
import akka.pattern.ask
import maker.akka.PartialFunctionUtils
import java.lang.management.ManagementFactory
import org.scalatest.tools.Runner

object RunTests {
  def main(args : Array[String]){
    println("Debug: " + (new java.util.Date()) + " RunTests: ")
    val result = Runner.run(args)
    SlaveTestSystem.system.awaitTermination()
    if (result)
      System.exit(0)
    else
      System.exit(1)

  }
}

object SlaveTestSystem{
  val port : Int = Option(System.getProperty(RemoteActor.localActorSystemPortLabel)).map(_.toInt).getOrElse{
    throw new RuntimeException("Local system port not set")
  }
    
  val system = {
    val config = {
      val text = """
        akka {
          loggers = ["akka.event.slf4j.Slf4jLogger"]
          loglevel = "ERROR"
          log-dead-letters = off
          log-dead-letters-during-shutdown = off
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
    val module = System.getProperty("maker.test.module")
    ActorSystem.create("REMOTE-" + module, config)
  }
  val actor : ActorRef = system.actorOf(Props[TestReporterActor], "test-reporter")
}

class AkkaTestReporter extends Reporter{

  val List(idString, host) = ManagementFactory.getRuntimeMXBean().getName().split("@").toList
  
  val actor = SlaveTestSystem.actor

  def apply(event : Event){
    try {
      event match {
        case e : Event =>
          actor ! event
      }
    } catch {
      case e : Throwable =>
        println("Exception when processing event " + event + " will shutdown")
        System.exit(1)
    }
  }
}

class TestReporterActor extends RemoteActor{

  var runComplete = false

  override def postStop{
    println("Debug: " + (new java.util.Date()) + " AkkaTestReporter: received poison pill - trying to die")
    context.system.shutdown
  }

  def activate(manager : ActorRef) = {
    def processEvents{
      
      toProcess.reverse.foreach{
        x => 
          try {
            x match {
              case (_, event : Event) =>
                manager ! event

              case other =>
                println(" AkkaTestReporter: unexpected event " + other)
              }
          } catch {
            case e : Throwable => 
              println("Error processing " + x + ", will exit")
              System.exit(1)
          }
      }
      toProcess = Nil
    }

    processEvents

    val pf : PartialFunction[Any, Unit] = {
      case event : Event =>
        toProcess = (sender, event) :: toProcess
        processEvents
      case other =>
        println(" ActorTestReporter: unexpected event " + other)
    }
    pf
  }
}


