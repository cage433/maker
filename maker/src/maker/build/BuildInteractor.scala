package maker.build

import akka.dispatch.Promise
import akka.actor.ActorRef

/** 
  * Sends any key presses to the build manager, 
  * shuts down as soon as the build has completed
  */
case class BuildInteractor(buildManager : ActorRef, buildPromise : Promise[_]) extends Runnable{
  import BuildInteractor.KeyPressed
  def run {
    try {
      while(! buildPromise.isCompleted){
        if (System.in.available > 0)
          buildManager ! KeyPressed(System.in.read)
        Thread.sleep(100)
      }
    } catch {
      case e : Throwable => 
        println("Got error " + e)
    }
  }
}

object BuildInteractor{
  case class KeyPressed(c : Int)

  def startAndJoin(buildManager : ActorRef, buildPromise : Promise[_]){
    val thread = new Thread(BuildInteractor(buildManager, buildPromise))
    thread.start
    thread.join
  }
}
