package starling.bouncyrmi

import scala.swing.event.Event
import java.io._
import java.util.zip.{Inflater, Deflater}
import org.apache.commons.io.input.ClassLoaderObjectInputStream
import org.jboss.netty.handler.ssl.SslHandler
import org.jboss.netty.handler.codec.oneone.OneToOneEncoder
import org.jboss.netty.channel._
import org.jboss.netty.handler.codec.compression.{ZlibDecoder, ZlibEncoder}
import org.jboss.netty.handler.codec.serialization.ObjectDecoder
import org.jboss.netty.handler.codec.serialization.ObjectEncoder
import org.jboss.netty.handler.timeout._
import org.jboss.netty.util.{HashedWheelTimer, Timer}
import org.jboss.netty.handler.execution.{OrderedMemoryAwareThreadPoolExecutor, ExecutionHandler}
import java.util.concurrent.{TimeUnit, Executors, ThreadFactory}
import starling.auth.User
import starling.utils.ClosureUtil._
import starling.utils.ImplicitConversions._
import java.lang.Boolean


//Message classes
case class MethodInvocationRequest(version: String, id: Int, method: String, parameters: Array[String], arguments: Array[Object])
case class MethodInvocationResult(id: Int, result: Object)
case class MethodInvocationException(id: Int, t: Throwable)
case class ServerException(t: Throwable)
case class MethodInvocationBadVersion(id: Int, serverVersion: String)
case class StdOutMessage(id: Int, line: String)
case class BroadcastMessage(event: Event)
case class ShutdownMessage(message: String)
case object VersionCheckRequest
case class VersionCheckResponse(version: String)
case class AuthMessage(ticket: Option[Array[Byte]], overriddenUser:Option[String])
case object AuthFailedMessage
case object AuthSuccessfulMessage
case object PingMessage
case object PongMessage

// Exceptions
class OfflineException(val message: String, t: Throwable) extends RuntimeException(message, t) {
  def this(message: String) = this (message, null)
}
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
  // who is logged in on this channel
  val loggedIn = new ChannelLocal[User]()

  val CodeVersionUndefined = "undefined"
  val CodeVersionKey = "starling.codeversion.timestamp"
  def CodeVersion = System.getProperty(CodeVersionKey, CodeVersionUndefined)

  val CertIssuerName = "CN=starling"

  def addCommon(pipeline: ChannelPipeline, timer:HashedWheelTimer, logger:(String)=>Unit) = {
    pipeline.addLast("compress", new MyCompressEncoder(Deflater.BEST_SPEED))
    pipeline.addLast("encoder", new ObjectEncoder())
    pipeline.addLast("decompress", new MyDecompressDecoder(200 * 1024 * 1024, logger))
    pipeline.addLast("decoder", new ObjectDecoder(200 * 1024 * 1024))
    pipeline.addLast("idle", new IdleStateHandler(timer, 0, 0, 5))
  }
}

class ServerPipelineFactory(authHandler: ServerAuthHandler, handler: SimpleChannelUpstreamHandler, timer:HashedWheelTimer, secured : Boolean = true) extends ChannelPipelineFactory {
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

class NamedDaemonThreadFactory(name: String) extends ThreadFactory {
  val sequence = new java.util.concurrent.atomic.AtomicInteger(0)

  def newThread(r: Runnable) = daemon(r).update(_.setName(sequence.getAndIncrement + " " + name))
}

// gzip
// time requests (cpu and clock)
// test stdout callback
// github
