package maker.utils

import akka.actor.Actor
import akka.actor.Props
import akka.actor.ActorSystem

object RemoteActorSpike extends App{

  class Doubler extends Actor{
    def receive = {
      case x : String =>
        sender ! (x + x)
      case x : Int =>
        sender ! (x + x)
    }
  }

  class Requester extends Actor{
    override def preStart(){
      val doubler = context.actorOf(Props[Doubler], "doubler")
      doubler ! "hello"
      doubler ! 34
    }

    def receive = {
      case any =>
        println("Debug: " + (new java.util.Date()) + " + RemoteActorSpike: received " + any)
    }
  }

  val system = ActorSystem.create
  val req = system.actorOf(Props[Requester], "requester")
  system.shutdown

}
