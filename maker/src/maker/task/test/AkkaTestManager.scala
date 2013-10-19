package maker.task.test

import com.typesafe.config.ConfigFactory
import akka.actor.ActorSystem
import akka.util.Timeout
import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.Props
import akka.actor.ExtendedActorSystem
import org.scalatest.events.RunCompleted
import maker.utils.MakerLog
import org.scalatest.events.Event
import org.scalatest.events.SuiteCompleted
import akka.pattern.Patterns
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import java.util.concurrent.TimeUnit
import org.scalatest.events.TestSucceeded

class AkkaTestManager extends TestResultsTrait{

  import AkkaTestManager._

  private val config = {
    val text = """
      akka {
        loggers = ["akka.event.slf4j.Slf4jLogger"]
        actor {
          provider = "akka.remote.RemoteActorRefProvider"
        }
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

  val system = ActorSystem.create("TestManager", config)

  val manager = system.actorOf(Props[Manager], "manager")

  val port = system.asInstanceOf[ExtendedActorSystem].provider.getDefaultAddress.port.get
  val address = system.asInstanceOf[ExtendedActorSystem].provider.getDefaultAddress

  private def askActor[T](msg : AnyRef) : T = {
    val fut = Patterns.ask(manager, msg, 10 * 1000)
    Await.result(fut, Duration(100, TimeUnit.SECONDS)).asInstanceOf[T]
  }

  def numCompleteSuites : Int = {
    askActor[Int](NUM_COMPLETE_SUITES)
  }

  def ++(rhs : TestResultsTrait) = CompositeTestResults(List(this, rhs))
  
  def succeeded() : Boolean = false
  def numPassedTests() : Int  = askActor(NUM_PASSED_TESTS)
  def numFailedTests() : Int = 0
  def failedTestSuites : List[String] = Nil
}

object AkkaTestManager{
  trait Message
  case object NUM_COMPLETE_SUITES
  case object NUM_PASSED_TESTS

  class Manager extends Actor{

    var events : List[Event] = Nil
    val log = MakerLog()
    var reporters : List[ActorRef] = Nil

    private def processRequest(sender : ActorRef, msg : Any){
      try {
        msg match {
          case ("REGISTER", module : String) =>
            reporters = sender :: reporters

          case e : RunCompleted =>
            events ::= e 
            sender ! "ACK"  

          case e : Event =>
            events ::= e 

          case NUM_COMPLETE_SUITES =>
            sender ! events.collect {
              case _ : SuiteCompleted => true
            }.size

          case NUM_PASSED_TESTS =>
            sender ! events.collect {
              case _ : TestSucceeded => true
            }.size

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

    override def postStop(){
      println("Debug: " + (new java.util.Date()) + " AkkaTestManager: stopping")
    }
  }

}
