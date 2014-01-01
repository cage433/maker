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
import scala.concurrent.duration._
import akka.pattern.ask
import java.lang.management.ManagementFactory
import org.scalatest.tools.Runner
import scala.collection.JavaConversions._
import maker.akka.RemoteActor

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
    
  val system = {
    val module = Option(System.getProperty("maker.test.module")).getOrElse{
      throw new RuntimeException("Property 'maker.test.module' not set")
    }
    ActorSystem.create("REMOTE-" + module, RemoteActor.systemConfig)
  }
  val actor : ActorRef = system.actorOf(Props[TestReporterActor], "test-reporter")
}

class AkkaTestReporter extends Reporter{

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

class TestReporterActor extends Actor{

  import TestReporterActor._
  val localSystemAddress = System.getProperty(localSystemAddressLabel)
  val module = System.getProperty("maker.test.module")
  val localActorPath = localSystemAddress + "/" + "user/test-manager-" + module
  val testManager = context.actorSelection(localActorPath) 

  var toProcess : List[Any] = Nil

  override def postStop{
    context.system.shutdown
  }

  def receive = {
    case msg : Any =>
      toProcess = msg :: toProcess
      processEvents
  }

  private def processEvents{
    toProcess.reverse.foreach{
      x => 
        try {
          x match {
            case event : Event =>
              testManager ! event

            case DumpTestThread => 
              println(TestReporterActor.someTestStackTrace())

            case other =>
              println(" AkkaTestReporter2: unexpected event is " + other)
            }
        } catch {
          case e : Throwable => 
            println("Error processing " + x + ", will exit")
            System.exit(1)
        }
    }
    toProcess = Nil
  }

}

object TestReporterActor{

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

  val localSystemAddressLabel = "maker.local.system.address"
  case object DumpTestThread

  /**
    * Dumps the stack trace of any running test - typically used when a test is hanging, in 
    * which case there will be only one
    */
  def someTestStackTrace() : String = {
    Thread.getAllStackTraces.values.toList.map(_.toList).filter{
      stackTraceList => 
        stackTraceList.map(_.toString).exists(_.contains("org.scalatest.tools.SuiteRunner"))
    }.headOption.map(_.mkString("\n\t", "\n\t", "\n")).getOrElse("")
  }
}
