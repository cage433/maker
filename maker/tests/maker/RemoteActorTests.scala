package maker

import org.scalatest.FunSuite
import akka.actor.ActorSystem
import akka.actor.ExtendedActorSystem
import com.twitter.util.Promise
import com.twitter.util.Duration
import java.util.concurrent.TimeUnit
import akka.actor.Actor

class Master extends Actor{
  def receive = {
    case "HI" =>
      sender ! "BYE"
    case other => 
      println("Debug: " + (new java.util.Date()) + " + RemoteActorTests: master received " + other)
      sender ! (other.toString + other.toString)
  }
}

class Slave(port : Int) extends Actor{
  def receive = {
    case any =>
      println("Debug: " + (new java.util.Date()) + " + RemoteActorTests: slave received " + any)
  }
}
class RemoteActorTests extends FunSuite {
  test("Test"){
    RemoteActorTests.run
  }
}

object RemoteActorTests extends App{
  def run{
    var systemPromise : Promise[ExtendedActorSystem] = Promise()

    val mainThread = new Thread(
      new Runnable(){
        def run(){
          val system = ActorSystem.create().asInstanceOf[ExtendedActorSystem]
          systemPromise.setValue(system)
          val actor = system.actorOf(akka.actor.Props[Master], "Master")
        }
      }
    )
    val thread1 = new Thread(
      new Runnable(){
        def run{
          val sys = systemPromise.get(Duration(100, TimeUnit.SECONDS))

        }
      }
    )
    mainThread.start
    val system = systemPromise.get(Duration(100, TimeUnit.SECONDS))()
// akka.tcp://default@127.0.0.1:54916]
    println(system.asInstanceOf[ExtendedActorSystem].provider.getDefaultAddress)
    println(system.asInstanceOf[ExtendedActorSystem].provider.getDefaultAddress.port.get.toInt)
    system.shutdown
  }
  run
}
