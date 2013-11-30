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

case class RemoteActorManager(system : ActorSystem){

}

object RemoteActorLauncher{
  def launch(localActorName : String, config : String) : Process = {
    null
  }
}

trait Receiver{
  def active(manager : ActorRef) : PartialFunction[Any, Unit]
}

case class RemoteActor(
  localActorSelection : ActorSelection, 
  localActorPath : String, 
  receiver : Receiver
) extends Actor{
  var toProcess : List[(ActorRef, Any)] = Nil
  private def sendIdentifyRequest(){
    localActorSelection ! Identify(localActorPath)
  }
  def receive = {
    case ActorIdentity(`localActorSelection`, Some(localActor)) =>
      context.setReceiveTimeout(Duration.Undefined)
      context.become(receiver.active(localActor))
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

object RemoteActorManager extends App{
  println("Hello")
  val system = MakerActorSystem.system
  println(MakerActorSystem.supervisor.path)

  
  private val configText = """
    akka {
      loggers = ["akka.event.slf4j.Slf4jLogger"]
      actor {
        provider = "akka.remote.RemoteActorRefProvider"
      }
      loglevel = off
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
  private val config = ConfigFactory.parseString(configText)

  def javaOpts(localSystem : ExtendedActorSystem, localActor : ActorRef) : List[String] = {

    val localPort = localSystem.provider.getDefaultAddress.port.get.toString
    val localActorName = localActor.path.name
    val localSystemName = localSystem.name
    List(
      "-Dmaker.local.system.port", localPort,
      "-Dmaker.local.system.name", localSystemName,
      "-Dmaker.local.actor.name", localActorName,
      "-Dmaker.remote.system.config", configText
    )
  }

  def create() {
    val localPort = System.getProperty("maker.local.system.port").toInt
    val localActorName = System.getProperty("maker.local.actor.name")
    val localSystemName = System.getProperty("maker.local.system.name")
    val remoteSystemName = "$localSystemName-remote"
    val remoteSystemConfig = System.getProperty("maker.remote.system.config")
    val localActorPath = s"akka.tcp://$localSystemName@127.0.0.1:$localPort/user/$localActorName"
    val receiveClassName = System.getProperty("maker.remote.receive.classname")
    val receive = Class.forName(receiveClassName).newInstance.asInstanceOf[PartialFunction[Any, Unit]]
    val system = ActorSystem.create(remoteSystemName, ConfigFactory.parseString(remoteSystemConfig))
    val localActorSelection = system.actorSelection(localActorPath)
  }
}
