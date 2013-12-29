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

class AkkaTestManager extends Actor{

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

        case AkkaTestManager.EVENTS =>
          sender ! events

        case "Hello" =>
          // Used to test remote test reporters

        case other =>
          log.error("Debug: " + (new java.util.Date()) + " AkkaTestManager: received " + other)
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

object AkkaTestManager{
  case object EVENTS
}
