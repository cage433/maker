package maker.task.test

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
import akka.actor.PoisonPill
import maker.utils.Implicits.RichString._
import org.scalatest.events.IndentedText

case class AkkaTestManager(system : ExtendedActorSystem, baseProject : BaseProject) {

  import AkkaTestManager._

  val name = "manager-" + baseProject.name + "-" + MakerActorSystem.nextActorID()
  val manager = system.actorOf(AkkaProps[Manager], name)
  val port : Int = system.provider.getDefaultAddress.port.get


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
  case object EVENTS

  class Manager extends Actor{

    val isRunningInMakerTest : Boolean = Props(file(".")).RunningInMakerTest()

    var events : List[Event] = Nil
    val log = MakerLog()

    private def processRequest(sender : ActorRef, msg : Any){
      try {
        msg match {

          case e : RunCompleted =>
            events ::= e 
            sender ! PoisonPill

          case e : Event =>
            events ::= e 

          case EVENTS =>
            sender ! events

          case "Hello" =>
            // Used to test remote test reporters

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
      //println("Debug: " + (new java.util.Date()) + " AkkaTestManager: received message " + msg + " from sender " + sender)
        processRequest(sender, msg)
    }
  }

}
