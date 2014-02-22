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
import akka.dispatch.Await
import akka.util.duration._
import akka.actor.ActorRef
import akka.actor.ReceiveTimeout
import akka.pattern.ask
import java.lang.management.ManagementFactory
import org.scalatest.tools.Runner
import scala.collection.JavaConversions._
import maker.akka.RemoteActor
import scala.util.Properties

object RunTests {
  def main(args : Array[String]){
    val result = Runner.run(args)
    TestReporterActor.system.awaitTermination()
    if (result)
      System.exit(0)
    else
      System.exit(1)

  }
}

class AkkaTestReporter extends Reporter{

  val actor = TestReporterActor.actor

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
  val testManager = context.actorFor(localSystemAddress + "/user/TestManager-" + module) 
  val buildManager = context.actorFor(localSystemAddress + "/user/BuildManager")

  override def postStop{
    context.system.shutdown
  }

  def receive = {
    case msg : Any =>
      processEvent(msg)
  }

  private def processEvent(msg : Any){
    try {
      msg match {
        case e : RunStarting =>
          buildManager ! ModuleTestsStarted(module)
          testManager ! e

        case e : RunAborted =>
          buildManager ! ModuleTestsFinished(module)
          testManager ! e

        case e : RunCompleted =>
          buildManager ! ModuleTestsFinished(module)
          testManager ! e

        case e : Event =>
          testManager ! e

        case DumpTestThread => 
          println(TestReporterActor.someTestStackTrace())

        case other =>
          println(" AkkaTestReporter: unexpected event is " + other)
        }
    } catch {
      case e : Throwable => 
        println("Error processing " + msg + ", will exit")
        System.exit(1)
    }
  }

}

object TestReporterActor{

  private def property(label : String) = Properties.propOrNone(label).getOrElse{
    throw new RuntimeException("Property " + label + " not set")
  }

  val localSystemAddress = property(RemoteActor.localSystemAddressLabel)
  val module = property("maker.test.module")
  val system = ActorSystem.create("REMOTE-" + module, RemoteActor.systemConfig)
  val actor : ActorRef = system.actorOf(Props[TestReporterActor], "test-reporter")
  case object DumpTestThread
  case class ModuleTestsStarted(moduleName : String)
  case class ModuleTestsFinished(moduleName : String)

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
