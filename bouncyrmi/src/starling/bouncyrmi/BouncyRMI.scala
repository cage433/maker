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
import java.lang.Boolean


//Message classes
case class MethodInvocationRequest(version: String, id: Int, declaringClass: String, method: String, parameters: Array[String], arguments: Array[Object])
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

trait LoggedIn[T,U] {
  def get(channel: Channel): (T,U)
  def set(channel: Channel, user: T, id:U): (T,U)
  def remove(channel: Channel): (T,U)
  def setLoggedOn(user: Option[T])
}

object BouncyRMI {
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

class ServerPipelineFactory[User](authHandler: ServerAuthHandler[User], handler: SimpleChannelUpstreamHandler, timer:HashedWheelTimer, secured : Boolean = true) extends ChannelPipelineFactory {
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

  def newThread(r: Runnable) = {
    val t = new Thread(r)
    t.setDaemon(true)
    t.setName(sequence.getAndIncrement + " " + name)
    t
  }
}