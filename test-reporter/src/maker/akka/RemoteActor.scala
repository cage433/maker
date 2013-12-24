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
import akka.actor.{Props => AkkaProps}
import akka.actor.Address

trait RemoteActor extends Actor
{
  import RemoteActor._
  val localActorPath = System.getProperty(localActorPathLabel)

  def activate(localActorRef : ActorRef) : PartialFunction[Any, Unit]

  def sendIdentifyRequest(){
    context.actorSelection(localActorPath) ! Identify(localActorPath)
  }

  var toProcess : List[(ActorRef, Any)] = Nil
  context.setReceiveTimeout(3.seconds)
  sendIdentifyRequest()

  def receive = {

    val pf : PartialFunction[Any, Unit] = {
      case ActorIdentity(`localActorPath`, Some(localActor)) =>
        
        context.setReceiveTimeout(Duration.Undefined)
        localActor ! "Hello"
        context.become(activate(localActor))

      case ActorIdentity(`localActorPath`, None) => 
        println(s"Remote actor not availible: $localActorPath")

      case ReceiveTimeout => 
        sendIdentifyRequest()

      case other => 
        toProcess = (sender, other) :: toProcess
    }
    PartialFunctionUtils.withExceptionsToStdOut(pf)
  }
}

object RemoteActor {

  private val remoteActorClassnameLabel ="maker.remote.actor.classname"
  val localActorPathLabel = "maker.local.actor.path"
  val localActorSystemPortLabel = "maker.local.actor.port"

  def javaOpts(localActor : ActorRef, localSystem : ExtendedActorSystem, remoteActorClassname : String) = {
    val elements = localActor.path.elements.mkString("/")
    val localActorPath = localSystem.provider.getDefaultAddress + "/" + elements
    val localPort = localSystem.provider.getDefaultAddress.port.get

    List(
      s"-D$localActorPathLabel=$localActorPath",
      s"-D$remoteActorClassnameLabel=$remoteActorClassname",
      s"-D$localActorSystemPortLabel=$localPort"
    )
  }
  def create() {
    val localActorClass = Class.forName(
      System.getProperty(remoteActorClassnameLabel)
    )
    val system = ActorSystem.create("Remote-system")
    val props = AkkaProps.create(localActorClass)
    val remoteActor = system.actorOf(props)
  }
}

