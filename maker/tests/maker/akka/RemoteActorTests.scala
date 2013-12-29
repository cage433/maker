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
import maker.utils.os.ScalaCommand
import maker.utils.os.CommandOutputHandler

object LaunchRemoteActorProcess extends App{

  // This runs in the remote process
  RemoteActor.create

  // This runs in the local one
  def start(
    props : Props, classpath : String, 
    localSystem : ExtendedActorSystem, localActor : ActorRef, 
    remoteActorClassname : String
  ) = {

    val props = Props(file("."))
    val cmd = ScalaCommand(
      CommandOutputHandler(),
      props.Java,
      RemoteActor.javaOpts(localActor, localSystem, remoteActorClassname),
      classpath,
      "maker.akka.LaunchRemoteActorProcess",
      name = "LaunchRemoteActor"
    )
    cmd.execAsync
  }
}

class TestRemoteActor extends RemoteActor{
  def activate(localActorRef : ActorRef) : PartialFunction[Any, Unit] = {
    case "stop" =>
      println("Shutting down")
      context.system.shutdown
  }
}

class RemoteActorTests 
extends TestKit(ActorSystem.create("MAKER-TEST-SYSTEM", MakerActorSystem.systemConfig))
with FunSpecLike 
with BeforeAndAfterAll
with ImplicitSender
{
  override def afterAll() { 
    system.shutdown() 
  }

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
    ignore("Should start up and register itself"){
      withTempDir{
        dir => 
        writeAkkaConfig(dir)
        val props : Props = Props.initialiseTestProps(dir)
          println("Debug: " + (new java.util.Date()) + " RemoteActorTests: " + props.MakerTestReporterClasspath())
        props.StopCompileOutput := false
        val classpath : String = dir.getAbsolutePath + "/" + ":" + 
        getClass.getClassLoader.asInstanceOf[URLClassLoader].getURLs.toList.map(_.getFile).mkString(":") 

        val localActor = TestProbe()

        val (proc, exitStatusFuture) = LaunchRemoteActorProcess.start(
          props, classpath, system.asInstanceOf[ExtendedActorSystem], 
          localActor.ref, "maker.akka.TestRemoteActor"
        )

        localActor.expectMsg("Hello")
        println("Debug: " + (new java.util.Date()) + " RemoteActorTests: replying with a stop")
        localActor.reply("stop")
        println("Debug: " + (new java.util.Date()) + " RemoteActorTests: awaiting exit status")
        println("Debug: " + (new java.util.Date()) + " RemoteActorTests: exit status is " + exitStatusFuture())
        assert(exitStatusFuture() === 0, "Future should return 0")

      }

    }
  }
}
