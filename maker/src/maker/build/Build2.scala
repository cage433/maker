package maker.build

import maker.utils.MakerLog
import akka.actor.Actor
import maker.akka.MakerActorSystem
import akka.actor.Props
import akka.pattern.ask
import scala.concurrent.duration._
import akka.util.Timeout

case class Build2(
  graph_ : () => Dependency.Graph
){

  lazy val graph = graph_()
  val log = MakerLog()

  class Manager extends Actor{
    def processRequest(msg : Any){
    }
    def receive = {
      case msg => 
        try {
          processRequest(msg)
        } catch {
          case e : Exception => 
            log.error("couldn't process " + msg, e)
        }
    }
  }


  def execute : BuildResult = {
    val manager = MakerActorSystem.system.actorOf(Props[Manager], "BuildManager")
    implicit val timeout = Timeout(10 seconds)
    val future = manager ? graph
    null
  }
}
