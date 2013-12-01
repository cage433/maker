package maker.akka


import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.ActorSystem
import akka.actor.ExtendedActorSystem
import akka.actor.{Props => AkkaProps}
import akka.testkit.ImplicitSender
import akka.testkit.TestKit
import akka.testkit.TestKitBase
import akka.testkit.TestProbe
import com.typesafe.config.ConfigFactory
import java.io.File
import java.net.URLClassLoader
import maker.project.TestModule
import maker.Props
import maker.utils.FileUtils._
import org.scalatest.BeforeAndAfterAll
import org.scalatest.FunSpecLike
import org.scalatest.FunSuite

class TestReceiver1 extends Receiver{
  def active(remoteSystem : ActorSystem, localActorRef : ActorRef) : PartialFunction[Any, Unit] = {
    case "stop" =>
      println("Shutting down")
      remoteSystem.shutdown
  }
}

class RemoteActorTests 
  extends TestKit(ActorSystem.create("MAKER-TEST-SYSTEM"))
  with FunSpecLike 
  with BeforeAndAfterAll
  with ImplicitSender
{
  override def afterAll() { system.shutdown() }

  def writeAkkaConfig(projectRoot : File){
    writeToFile(
      file(projectRoot, "application.conf"), 
      """
        akka {
          loggers = ["akka.event.slf4j.Slf4jLogger"]
          actor {
            provider = "akka.remote.RemoteActorRefProvider"
          }
          log-dead-letters = off
          log-dead-letters-during-shutdown = off
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
          val props : Props = Props.initialiseTestProps(dir)
          props.StopCompileOutput := false
          val classpath : String = dir.getAbsolutePath + "/" + ":" + 
            getClass.getClassLoader.asInstanceOf[URLClassLoader].getURLs.toList.map(_.getFile).mkString(":") 

          val localActor = TestProbe()

          val (proc, exitStatusFuture) = RemoteActor.start(
            props, classpath, system.asInstanceOf[ExtendedActorSystem], 
            localActor.ref, "maker.akka.TestReceiver1"
          )

          localActor.expectMsg("Hello")
          localActor.reply("stop")
          assert(exitStatusFuture() === 0, "Future should return 0")

      }

    }
  }
}
