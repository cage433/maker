package starling.bouncyrmi

import scala.swing.event.Event
import java.util.zip.{Inflater, Deflater}
import org.apache.commons.io.input.ClassLoaderObjectInputStream
import org.jboss.netty.handler.ssl.SslHandler
import org.jboss.netty.handler.codec.oneone.OneToOneEncoder
import org.jboss.netty.channel._
import org.jboss.netty.handler.codec.compression.{ZlibDecoder, ZlibEncoder}
import org.jboss.netty.handler.timeout._
import org.jboss.netty.util.{HashedWheelTimer, Timer}
import org.jboss.netty.handler.execution.{OrderedMemoryAwareThreadPoolExecutor, ExecutionHandler}
import java.util.concurrent.{TimeUnit, Executors, ThreadFactory}
import java.lang.Boolean
import java.io._
import org.jboss.netty.handler.codec.serialization.{ObjectEncoderOutputStream, ObjectDecoderInputStream, ObjectDecoder, ObjectEncoder}
import starling.utils.NamedDaemonThreadFactory
import starling.manager.TimeTree
import starling.auth.{Client, AuthHandler}

//Message classes
case class MethodInvocationRequest(version: String, id: Int, declaringClass: String, method: String, parameters: Array[String], arguments: Array[Array[Byte]]) {
  if (declaringClass == classOf[Object].getName) {
    throw new IllegalArgumentException("Invoking against Object not allowed: " + this)
  }
}
case class MethodInvocationResult(id: Int, serverDuration:Long, timeTree:TimeTree, result:Array[Byte])
case class MethodInvocationException(id: Int, t: Throwable)
case class ServerException(t: Throwable)
case class MethodInvocationBadVersion(id: Int, serverVersion: String)
case class StdOutMessage(id: Int, line: String)
case class BroadcastMessage(event: Event)
case class ShutdownMessage(message: String)
case object VersionCheckRequest
case class VersionCheckResponse(version: String)
case class AuthMessage(ticket: Option[Array[Byte]], overriddenUser:Option[String]) {
  assert(ticket != null)
}
case object AuthFailedMessage
case object AuthSuccessfulMessage
case object PingMessage
case object PongMessage

// Exceptions
class OfflineException(val message: String, t: Throwable) extends RuntimeException(message, t) {
  def this(message: String) = this (message, null)
}
// If the client can't connect but it's probably not the sever's fault
class ClientOfflineException(val message: String) extends RuntimeException(message)
class CannotConnectException(val message: String, t: Throwable) extends RuntimeException(message, t)
class ServerUpgradeException(val serverVersion: String) extends RuntimeException("Cannot reconnect as Starling has been upgraded. Please restart this application. " + serverVersion + " : " + BouncyRMI.CodeVersion)
class AuthFailedException(val msg: String) extends RuntimeException()

// Events
case object ClientConnectedEvent
case object ClientDisconnectedEvent
case class UnexpectedDisconnectEvent(t: Throwable)

class ClientPipelineFactory(handler: SimpleChannelHandler, timer:HashedWheelTimer, logger:(String)=>Unit) extends ChannelPipelineFactory {
  def getPipeline() = {
    val pipeline = Channels.pipeline
    val engine = SslContextFactory.clientContext.createSSLEngine
    engine.setUseClientMode(true)

    pipeline.addLast("ssl", new SslHandler(engine))
    BouncyRMI.addCommon(pipeline, timer, logger)
    //    pipeline.addLast("timeout", new ReadTimeoutHandler(BouncyRMI.timer, 10))
    pipeline.addLast("handler", handler)

    pipeline
  }
}

object BouncyRMI {
  val CodeVersionUndefined = "undefined"
  val CodeVersionKey = "starling.codeversion"
  def CodeVersion = System.getProperty(CodeVersionKey, CodeVersionUndefined)

  val CertIssuerName = "CN=starling"

  def addCommon(pipeline: ChannelPipeline, timer:HashedWheelTimer, logger:(String)=>Unit) = {
    pipeline.addLast("compress", new MyCompressEncoder(Deflater.BEST_SPEED))
    pipeline.addLast("encoder", new ObjectEncoder())
    pipeline.addLast("decompress", new MyDecompressDecoder(200 * 1024 * 1024, logger))
    pipeline.addLast("decoder", new ObjectDecoder(200 * 1024 * 1024, getClass.getClassLoader))
    pipeline.addLast("idle", new IdleStateHandler(timer, 0, 0, 5))
  }

  def encode(obj:Object) = {
    val bout = new ByteArrayOutputStream()
    val out = new ObjectEncoderOutputStream(bout)
    out.writeObject(obj)
    out.flush
    out.close
    bout.toByteArray
  }

  def decode(classLoader:ClassLoader, data:Array[Byte]) = {
    val in = new ObjectDecoderInputStream(new ByteArrayInputStream(data), classLoader, 200 * 1024 * 1024)
    in.readObject()
  }
}

class ServerPipelineFactory[User](classLoader:ClassLoader, authHandler: ServerAuthHandler, handler: SimpleChannelUpstreamHandler, timer:HashedWheelTimer, secured : Boolean = true) extends ChannelPipelineFactory {
  def getPipeline() = {
    val pipeline = Channels.pipeline
    val engine = SslContextFactory.serverContext.createSSLEngine
    engine.setUseClientMode(false)

    if (secured == true) pipeline.addLast("ssl", new SslHandler(engine))
    BouncyRMI.addCommon(pipeline, timer, (x)=>{})
    pipeline.addLast("threadPool", new ExecutionHandler(Executors.newCachedThreadPool(new NamedDaemonThreadFactory("Server worker"))))
    if (secured == true) pipeline.addLast("authHandler", authHandler)
    pipeline.addLast("handler", handler)
    pipeline
  }
}


//Code for running a dummy bouncy rmi client and server,
// Used for manually testing the reconnect behaviour after protocol level code changes
trait DummyService {
  def bob():String
}

object SimpleBouncyRmiServer {
  def main(args : Array[String]) {
    val s = new BouncyRMIServer(5000, AuthHandler.Dev, "v", Set())
    s.addInstance(classOf[DummyService], new DummyService() { def bob = "xxx"; })
    s.start
    Thread.sleep(10*10*1000)
  }
}

object SimpleBouncyRmiClient {
  def main(args : Array[String]) {
    System.setProperty(BouncyRMI.CodeVersionKey, "v")
    val c = new BouncyRMIClient("localhost", 5000, Client.Null)
    c.startBlocking()
    val dummy = c.proxy(classOf[DummyService])
    while (true) {
      try {
        println("Bob: " + dummy.bob)
      } catch {
        case e:Exception => println("Failed: " + e)
      }
      Thread.sleep(5000)
    }
  }
}