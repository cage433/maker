package starling.bouncyrmi

import scala.swing.event.Event
import java.nio.channels.ClosedChannelException
import org.jboss.netty.handler.ssl.SslHandler
import org.jboss.netty.channel.group.{ChannelGroupFuture, ChannelGroupFutureListener, DefaultChannelGroup}
import org.jboss.netty.util.HashedWheelTimer
import java.net.InetSocketAddress;
import java.util.concurrent.Executors;
import org.jboss.netty.bootstrap.ServerBootstrap;
import org.jboss.netty.channel.socket.nio.NioServerSocketChannelFactory;
import org.jboss.netty.channel.ChannelFuture;
import org.jboss.netty.channel.ChannelFutureListener;
import org.jboss.netty.channel.ChannelHandlerContext;
import org.jboss.netty.channel.ChannelStateEvent;
import org.jboss.netty.channel.ExceptionEvent;
import org.jboss.netty.channel.MessageEvent;
import org.jboss.netty.channel.SimpleChannelUpstreamHandler;

import java.lang.reflect.InvocationTargetException
import collection.mutable.HashMap
import java.util.UUID


class BouncyRMIServer[User](val port: Int, authHandler: ServerAuthHandler[User], version: String, users:java.util.Map[UUID,User],
                            knownExceptionClasses:Set[Class[_]], loggedIn: LoggedIn[User,UUID], val name: String, serverContexts: AnyRef*) {

  lazy val serverTimer = new HashedWheelTimer
  private val serverContextsMap = new HashMap[String, AnyRef]
  
  class Binding {
    val group = new DefaultChannelGroup("server")
    val bootstrap = new ServerBootstrap(
      new NioServerSocketChannelFactory(
        Executors.newCachedThreadPool(new NamedDaemonThreadFactory("ServerA")),
        Executors.newCachedThreadPool(new NamedDaemonThreadFactory("ServerB"))));
    bootstrap.setPipelineFactory(new ServerPipelineFactory[User](getClass.getClassLoader, authHandler, new ServerHandler(group), serverTimer))
    val channel = bootstrap.bind(new InetSocketAddress(port))
    val bound = false

    def broadcast(obj: Object) = {
      import scala.collection.JavaConversions._
      val groupFuture = group.write(obj)
      groupFuture.addListener(new ChannelGroupFutureListener {
        def operationComplete(future: ChannelGroupFuture) = {
          if (future.isPartialFailure) {
            for (f <- future.iterator) {
              if (!f.isSuccess) {
                // SSLEngine already closed happens if the client has disconnected but we haven't
                // registered a disconnect yet. it seems to just be a timing issue and we'll get
                // the disconnect message very soon
                if (f.getCause.getMessage != "SSLEngine already closed") {
                  Logger.warn("Server: Failed to send", f.getCause)
                }
              }
            }
          }
        }
      })
      groupFuture
    }

    def stop(message: String) {
      Logger.info("Server: stopping with message: " + message)
      val shutdownBroadcast = broadcast(ShutdownMessage(message))
      shutdownBroadcast.await(2000)
      group.close().awaitUninterruptibly()
      channel.disconnect().awaitUninterruptibly()
      channel.close().awaitUninterruptibly()
      channel.unbind().awaitUninterruptibly()
      bootstrap.releaseExternalResources()
    }
  }

  var binding: Option[Binding] = None

  def start = {
    binding match {
      case Some(_) =>
      case None => binding = Some(new Binding)
    }
  }

  def publish(event: Event) {
    binding.map(_.broadcast(BroadcastMessage(event)))
  }

  def isStarted = binding != None

  def stop(message: String = "server shutdown") = {
    binding match {
      case None =>
      case Some(b) => {
        binding = None
        b.stop(message)
      }
    }
    serverTimer.stop
  }

  def getServerContext(declaringClassName: String): AnyRef = {
    def findAssignableContext = {
      val declaringClass = Class.forName(declaringClassName)

      serverContexts.find(context => declaringClass.isAssignableFrom(context.getClass))
        .getOrElse(throw new Exception("Unknown service: " + declaringClassName))
    }

    serverContextsMap.getOrElseUpdate(declaringClassName, findAssignableContext)
  }

  class ServerHandler(group: DefaultChannelGroup) extends SimpleChannelUpstreamHandler {
    override def channelConnected(ctx: ChannelHandlerContext, e: ChannelStateEvent) = {
      val sslHandler = ctx.getPipeline().get(classOf[SslHandler])
      Logger.info("Server: Channel connected. Waiting for SSL")
      // Get notified when SSL handshake is done.
      val handshakeFuture = sslHandler.handshake
      handshakeFuture.addListener(new ChannelFutureListener {
        def operationComplete(future: ChannelFuture) = {
          if (future.isSuccess) {
            Logger.info("Server: SSL Complete. Client has connected")
            group.add(future.getChannel)
          } else {
            Logger.info("Server: SSL Failed. Client failed to connect")
            future.getChannel.close
          }
        }
      })
    }

    override def channelDisconnected(ctx: ChannelHandlerContext, e: ChannelStateEvent) {
      val user = loggedIn.get(e.getChannel)
      Logger.info("Server: Client disconnected: " + e.getChannel.getRemoteAddress + " (" + user._1 + ")")
      users.remove(user._2)
      group.remove(e.getChannel)
      loggedIn.remove(e.getChannel)
    }

    override def messageReceived(ctx: ChannelHandlerContext, e: MessageEvent) {
      val message = e.getMessage
      message match {
        case VersionCheckRequest => {
          Logger.info("Sending version check response: " + version)
          e.getChannel.write(VersionCheckResponse(version))
        }
        case PingMessage => {
          e.getChannel.write(PongMessage)
        }
        case MethodInvocationRequest(clientVersion, id, declaringClassName, name, params, args) => {
          def write(result: AnyRef) {
            val future = e.getChannel.write(result)
            future.addListener(new ChannelFutureListener() {
              def operationComplete(channel: ChannelFuture) {
                if (!future.isSuccess) {
                  val result = MethodInvocationException(id, future.getCause)
                  e.getChannel.write(result)
                }
              }
            })
          }

          if (clientVersion != version) {
            write(MethodInvocationBadVersion(id, version))
          } else {
            val buffer = new StringBuilder
            val paramClasses = params.map(classForNameWithPrimitiveCheck(_))

            def makeExceptionForClient(t : Throwable) : MethodInvocationException = {
              // No way currently of knowing whether the exception contains
              // objects outside the modules the client knows about.
              // This might be fixable when we rename our packages to starling.module....

              // We want to pass through certain exceptions that we know are on the gui class path because we match on them.
              if (knownExceptionClasses.contains(t.getClass)) {
                MethodInvocationException(id, t)
              } else {
                val exceptionWithoutStarlingData = new Exception(t.getClass + "\n" + t.getMessage)
                exceptionWithoutStarlingData.setStackTrace(t.getStackTrace)
                MethodInvocationException(id, exceptionWithoutStarlingData)
              }
            }

            val result = try {
              val serverContext = getServerContext(declaringClassName)

              val method = serverContext.asInstanceOf[AnyRef].getClass.getMethod(name, paramClasses: _*)
              StdOut.setTee(line => e.getChannel.write(StdOutMessage(id, line)))
              val r = method.invoke(serverContext, args: _ *)
              StdOut.reset
              MethodInvocationResult(id, r)
            } catch {
              case e: InvocationTargetException => {
                e.getCause.printStackTrace()
                makeExceptionForClient(e.getCause)
              }
              case t: Throwable => {
                t.printStackTrace()
                makeExceptionForClient(t)
              }
            } finally {
              StdOut.setTee(c => {})
            }

            write(result)
          }
        }
        case _ =>
      }
    }

    private def classForNameWithPrimitiveCheck(className: String): Class[_] = {
      className match {
        case "int" => java.lang.Integer.TYPE
        case "double" => java.lang.Double.TYPE
        case "boolean" => java.lang.Boolean.TYPE
        case "long" => java.lang.Long.TYPE
        case "float" => java.lang.Float.TYPE
        case "short" => java.lang.Short.TYPE
        case "char" => java.lang.Character.TYPE
        case "byte" => java.lang.Byte.TYPE
        case _ => Class.forName(className)
      }
    }

    private def containsClosedChannelException(e: Throwable): Boolean = {
      var ee = e
      while (ee != null) {
        if (ee.isInstanceOf[ClosedChannelException]) return true
        ee = ee.getCause
      }
      false
    }

    override def exceptionCaught(ctx: ChannelHandlerContext, e: ExceptionEvent) {
      if ("Connection reset by peer" == e.getCause.getMessage) {
        e.getChannel().close()
      } else if ("Received fatal alert: certificate_unknown" == e.getCause.getMessage) {
        e.getChannel().close()
      } else if (containsClosedChannelException(e.getCause)) {
        //There is no point sending the stacktrace to the client if the channel is closed
        e.getChannel().close();
      } else {
        Logger.error("Server exception ", e.getCause())
        //Send the stacktrace to the client, then disconnect
        val future = ctx.getChannel.write(ServerException(e.getCause))
        future.addListener(new ChannelFutureListener() {
          def operationComplete(channel: ChannelFuture) {
            e.getChannel().close()
          }
        })
      }
    }
  }
}
