package maker.task.test

import com.typesafe.config.ConfigFactory
import akka.util.Timeout
import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.{Props => AkkaProps}
import akka.actor.ExtendedActorSystem
import org.scalatest.events._
import maker.utils.MakerLog
import akka.pattern.Patterns
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import java.util.concurrent.TimeUnit
import org.scalatest.events.TestSucceeded
import akka.actor.Terminated
import maker.project.BaseProject
import maker.akka.MakerActorSystem
import maker.utils.FileUtils._
import maker.Props

class AkkaTestManager(baseProject : BaseProject) {

  import AkkaTestManager._

  private val config = {
    val text = """
      akka {
        loggers = ["akka.event.slf4j.Slf4jLogger"]
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


  val name = "manager-" + baseProject.name + "-" + MakerActorSystem.nextActorID()
  val manager = MakerActorSystem.system.actorOf(AkkaProps[Manager], name)
  val port = MakerActorSystem.port


  private def askActor[T](msg : AnyRef) : T = {
    val fut = Patterns.ask(manager, msg, 10 * 1000)
    Await.result(fut, Duration(100, TimeUnit.SECONDS)).asInstanceOf[T]
  }

  def testResults() = {
    val events : List[Event] = askActor(EVENTS)
    TestResults(Map[String, List[Event]](baseProject.name -> events))
  }
}

object AkkaTestManager{
  trait Message
  case object NUM_COMPLETE_SUITES
  case object NUM_PASSED_TESTS
  case object NUM_FAILED_TESTS
  case object FAILED_TEST_SUITES
  case object IS_COMPLETE
  case object RESET
  case object EVENTS

  class Manager extends Actor{

    val isRunningInMakerTest : Boolean = Props(file(".")).RunningInMakerTest()
    var events : List[Event] = Nil
    val log = MakerLog()
    var reporters : List[ActorRef] = Nil

    private def processRequest(sender : ActorRef, msg : Any){
      try {
        msg match {

          case e : RunCompleted =>
            events ::= e 
            sender ! "stop"  

          case e : Event =>
            events ::= e 

          case EVENTS =>
            sender ! events
            
          case NUM_COMPLETE_SUITES =>
            sender ! events.collect {
              case _ : SuiteCompleted => true
            }.size

          case NUM_PASSED_TESTS =>
            sender ! events.collect {
              case _ : TestSucceeded => true
            }.size

          case NUM_FAILED_TESTS =>
            sender ! events.collect {
              case _ : TestFailed => true
            }.size

          case FAILED_TEST_SUITES =>
            sender ! events.collect {
              case t : TestFailed => t.suiteClassName.get
            }.toList.distinct

          case IS_COMPLETE =>
            sender ! events.collect {
              case _ : RunCompleted => true
            }.nonEmpty

          case RESET =>
            events = Nil
            reporters = Nil

          case other =>
            println("Debug: " + (new java.util.Date()) + " AkkaTestManager: received " + other)
        }
      } catch {
        case e : Throwable =>
          log.error("Error processing message " + msg + " from " + sender, e)
      }
    }

    def receive = {
      case msg : Any =>
        processRequest(sender, msg)
    }
  }

}
