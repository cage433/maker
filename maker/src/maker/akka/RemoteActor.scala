package maker.akka

import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory
import akka.actor.ActorRef
import akka.actor.ExtendedActorSystem
import akka.actor.ActorSystem
import akka.actor.Identify
import akka.actor.ActorIdentity
import akka.actor.ReceiveTimeout
import akka.actor.Actor
import scala.concurrent.Await
import scala.concurrent.duration._
import akka.actor.ActorSelection
import maker.utils.os.CommandOutputHandler
import maker.Props
import akka.actor.{Props => AkkaProps}
import maker.utils.os.ScalaCommand
import maker.utils.FileUtils._
import maker.project.Module
import akka.actor.Address


trait Receiver{
  def active(remoteSystem : ActorSystem, manager : ActorRef) : PartialFunction[Any, Unit]
}

case class RemoteActor(
  localActorPath : String, 
  receiver : Receiver
) 
  extends Actor
{

  private def sendIdentifyRequest(){
    context.actorSelection(localActorPath) ! Identify(localActorPath)
  }

  var toProcess : List[(ActorRef, Any)] = Nil
  context.setReceiveTimeout(3.seconds)
  sendIdentifyRequest()

  def processRequest(msg : Any) = {
    msg match {
      case ActorIdentity(`localActorPath`, Some(localActor)) =>
        
        context.setReceiveTimeout(Duration.Undefined)
        localActor ! "Hello"
        context.become(receiver.active(context.system, localActor))
        toProcess.reverse.foreach{
          case (sender, msg) => 
            self ! ((sender, msg))
        }

      case ActorIdentity(`localActorPath`, None) => 
        println(s"Remote actor not availible: $localActorPath")

      case ReceiveTimeout => 
        sendIdentifyRequest()

      case other => 
        toProcess = (sender, other) :: toProcess
    }
  }
  def receive = {
    case any => processRequest(any)
  }
}

object RemoteActor extends App{

  create

  def props(
    localActorPath : String, 
    receiver : Receiver
  ) = {
    AkkaProps.create(classOf[RemoteActor], localActorPath, receiver)
  }

  def start(
    props : Props, classpath : String, 
    system : ExtendedActorSystem, localActor : ActorRef, 
    remoteReceiverClass : String
  ) = {

    val props = Props(file("."))
    val cmd = ScalaCommand(
      CommandOutputHandler(),
      props.Java,
      javaOpts(system, localActor, remoteReceiverClass),
      classpath,
      "maker.akka.RemoteActor",
      "LaunchRemoteActor"
    )
    cmd.execAsync
  }
  
  def javaOpts(localSystem : ExtendedActorSystem, localActor : ActorRef, remoteReceiverClass : String) : List[String] = {

    val elements = localActor.path.elements.mkString("/")
    val localActorPath = localSystem.provider.getDefaultAddress + "/" + elements
    List(
      s"-Dmaker.local.actor.path=$localActorPath",
      s"-Dmaker.remote.receive.classname=$remoteReceiverClass"
    )
  }

  def create() {
    val localActorPath = System.getProperty("maker.local.actor.path")
    val receive = Class.forName(
      System.getProperty("maker.remote.receive.classname")
    ).newInstance.asInstanceOf[Receiver]
    val system = ActorSystem.create("Remote-system")
    val localActorSelection = system.actorSelection(localActorPath)
    val remoteActor = system.actorOf(RemoteActor.props(localActorPath, receive))
  }
}
