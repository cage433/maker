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
import scala.collection.JavaConversions._

object RunTests {
  def main(args : Array[String]){
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

  import TestReporterActor._

  var runComplete = false

  override def postStop{
    context.system.shutdown
  }

  def activate(manager : ActorRef) = {
    def processEvents{
      
      toProcess.reverse.foreach{
        x => 
          try {
            x match {
              case event : Event =>
                manager ! event

              case DumpTestThread => 
                println(TestReporterActor.someTestStackTrace())

              case other =>
                println(" AkkaTestReporter: unexpected event is " + other)
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
      case msg : Any =>
        toProcess = msg :: toProcess
        processEvents
    }
    pf
  }
}

object TestReporterActor{

  case object DumpTestThread

  /**
    * Dumps the stack trace of any running test - typically used when a test is hanging, in 
    * which case there will be only one
    */
  def someTestStackTrace() : String = {
    val testTrace = Thread.getAllStackTraces.values.toList.map(_.toList).filter{
      stackTraceList => 
        stackTraceList.map(_.toString).exists(_.contains("org.scalatest.tools.SuiteRunner"))
    }.take(1)
    testTrace.mkString("\n\t", "\n\t", "\n")
  }
}
