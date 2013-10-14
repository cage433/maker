package maker.utils

import akka.actor.Actor
import akka.actor.Props
import akka.actor.ActorSystem
import maker.utils.os.ScalaCommand
import maker.utils.os.CommandOutputHandler
import maker.utils.FileUtils._
import com.typesafe.config.ConfigFactory
import akka.util.Timeout
import scala.concurrent.Await
import akka.pattern.ask

object Config{
def apply(port : Int) = {
  val confText = s"""
akka {
  loggers = ["akka.event.slf4j.Slf4jLogger"]
  //loglevel = "DEBUG"
  actor {
    provider = "akka.remote.RemoteActorRefProvider"
  }
  remote {
    enabled-transports = ["akka.remote.netty.tcp"]
    netty.tcp {
      hostname = "127.0.0.1"
      port = $port
    }
    //log-received-messages = on
    //log-sent-messages = on
 }
}
"""
    ConfigFactory.parseString(confText)
  }
}
object RemoteSender extends App{
  implicit val timeout = Timeout(60000 * 30) // 30 minutes
  val system = ActorSystem.create("REMOTE", Config(2553))
  println("Debug: " + (new java.util.Date()) + " RemoteActorSpike: launched slave")
  val doublerPath = "akka.tcp://LOCAL@127.0.0.1:2552/user/doubler"
  val doublerFuture = system.actorSelection(doublerPath).resolveOne(timeout.duration)
  val doubler = Await.result(doublerFuture, timeout.duration)
 
  println("Debug: " + (new java.util.Date()) + " RemoteActorSpike: sending messages from remote")
  println("Debug: " + (new java.util.Date()) + " RemoteActorSpike: " + doubler)
  val promise1 = (doubler ? "hello from remote")
  val result1 = Await.result(promise1, timeout.duration)
  println("Debug: " + (new java.util.Date()) + " RemoteActorSpike: " + result1)
}

object RemoteActorSpike extends App{

  val system = ActorSystem.create("LOCAL", Config(2552))

  class Doubler extends Actor{
    def receive = {
      case x : String =>
        sender ! (x + x)
      case x : Int =>
        sender ! (x + x)
    }
  }

  val myDoubler = system.actorOf(Props[Doubler], "doubler")

  val props = maker.Props.apply(file("."))
  val cmd = ScalaCommand(
    props,
    CommandOutputHandler(),
    props.Java,
    Nil,
    System.getProperty("java.class.path"),
    "maker.utils.RemoteSender",
    "Launch slave",
    Nil
  )
  cmd.execAsync


  class LocalSender extends Actor{
    override def preStart(){
      val doublerPath = "akka.tcp://LOCAL@127.0.0.1:2552/user/doubler"
      val doubler = context.actorSelection(doublerPath)
      doubler ! "hello from local"
      doubler ! 34
    }

    def receive = {
      case any =>
        println("Debug: " + (new java.util.Date()) + " + RemoteActorSpike: received " + any)
    }
  }


  val req = system.actorOf(Props[LocalSender], "requester")
    //system.shutdown

}
