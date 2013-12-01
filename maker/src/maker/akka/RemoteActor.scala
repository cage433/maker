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


trait Receiver{
  def active(remoteSystem : ActorSystem, manager : ActorRef) : PartialFunction[Any, Unit]
}

case class RemoteActor(
  localActorSelection : ActorSelection, 
  localActorPath : String, 
  receiver : Receiver
) extends Actor{
  var toProcess : List[(ActorRef, Any)] = Nil
  println("Debug: " + (new java.util.Date()) + " RemoteActor: created " + this)
  private def sendIdentifyRequest(){
    localActorSelection ! Identify(localActorPath)
  }
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

object RemoteActorLauncher extends App{
  RemoteActor.create
}
object RemoteActor extends App{
  val system = MakerActorSystem.system
  println(MakerActorSystem.supervisor.path)

  def props(
    localActorSelection : ActorSelection, 
    localActorPath : String, 
    receiver : Receiver
  ) = {
    AkkaProps.create(classOf[RemoteActor], localActorSelection, localActorPath, receiver)
  }

  def start(module : Module, system : ExtendedActorSystem, localActor : ActorRef, remoteReceiverClass : String) = {
    
    val outputHandler = CommandOutputHandler().withSavedOutput
    val props = Props(file("."))
    val cmd = ScalaCommand(
      module.props,
      outputHandler,
      module.props.Java,
      javaOpts(system, localActor, remoteReceiverClass),
      module.testClasspath,
      "maker.akka.RemoteActorLauncher",
      "LaunchRemoteActor"
    )
    cmd.execAsync
  }
  
  def javaOpts(localSystem : ExtendedActorSystem, localActor : ActorRef, remoteReceiverClass : String) : List[String] = {

    val localPort = localSystem.provider.getDefaultAddress.port.get.toString
    val localActorName = localActor.path.name
    val localSystemName = localSystem.name
    List(
      s"-Dmaker.local.system.port=$localPort",
      s"-Dmaker.local.system.name=$localSystemName",
      s"-Dmaker.local.actor.name=$localActorName",
      s"-Dmaker.remote.receive.classname=$remoteReceiverClass"
    )
  }

  def create() {
    val localPort = System.getProperty("maker.local.system.port").toInt
    val localActorName = System.getProperty("maker.local.actor.name")
    val localSystemName = System.getProperty("maker.local.system.name")
    val remoteSystemName = s"$localSystemName-remote"
    val localActorPath = s"akka.tcp://$localSystemName@127.0.0.1:$localPort/system/$localActorName"
    val receiveClassName = System.getProperty("maker.remote.receive.classname")
    val receive = Class.forName(receiveClassName).newInstance.asInstanceOf[Receiver]
    val system = ActorSystem.create(remoteSystemName)
    val localActorSelection = system.actorSelection(localActorPath)
    val remoteActor = system.actorOf(RemoteActor.props(localActorSelection, localActorPath, receive))
  }
}
