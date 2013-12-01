package maker.akka

import org.scalatest.FunSuite
import akka.testkit.TestKitBase
import akka.actor.ActorSystem
import com.typesafe.config.ConfigFactory
import akka.actor.ExtendedActorSystem
import org.scalatest.FunSpecLike
import akka.actor.ActorRef
import akka.actor.Actor
import akka.actor.{Props => AkkaProps}
import maker.utils.FileUtils._
import maker.Props
import maker.project.TestModule
import akka.testkit.TestProbe
import org.scalatest.BeforeAndAfterAll
import java.io.File
import akka.testkit.ImplicitSender
import akka.testkit.TestKit


class TestReceiver1 extends Receiver{
  def active(remoteSystem : ActorSystem, localActorRef : ActorRef) : PartialFunction[Any, Unit] = {
    case "hi" => 
      localActorRef ! "hi to you too!"
    case "stop" =>
      println("Debug: " + (new java.util.Date()) + " RemoteActorTests: shutting down")
      remoteSystem.shutdown
  }
}

class DumbLocalActor extends Actor{
  def receive = {
    case any => 
      println(any)
  }
}

class RemoteActorTests 
  extends TestKit(ActorSystem.create("MAKER-TEST-SYSTEM"))
  with FunSpecLike 
  with BeforeAndAfterAll
  with ImplicitSender
{
  //implicit lazy val system = ActorSystem.create("MAKER-TEST-SYSTEM")
  override def afterAll() { system.shutdown() }

  def writeAkkaConfig(projectRoot : File){
    writeToFile(
      file(projectRoot, "/test-resources/application.conf"), 
      """
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
    )
  }
  describe("RemoteActor"){
    it("Should start up and register itself"){
      withTempDir{
        dir => 
          writeAkkaConfig(dir)
          val props = Props.initialiseTestProps(dir)
          val module = TestModule(dir, "RemoteActorTestModule", props)
          val localActor = TestProbe()
          val (proc, exitStatus) = RemoteActor.start(
            module, system.asInstanceOf[ExtendedActorSystem], 
            localActor.ref, "maker.akka.TestReceiver1")
          localActor.expectMsg("Hello")
          localActor.reply("stop")
      }

    }
  }
}
