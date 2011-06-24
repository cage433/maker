package starling.bouncyrmi

import scala.swing.event.Event

import org.jboss.netty.bootstrap.ClientBootstrap
import org.jboss.netty.channel.socket.nio.NioClientSocketChannelFactory
import net.sf.cglib.proxy.MethodProxy
import net.sf.cglib.proxy.MethodInterceptor
import net.sf.cglib.proxy.Enhancer
import java.lang.reflect.Method
import org.jboss.netty.channel._
import java.net.InetSocketAddress
import org.jboss.netty.handler.ssl.SslHandler
import javax.net.ssl.SSLHandshakeException
import java.util.concurrent._
import atomic.{AtomicBoolean, AtomicReference, AtomicInteger}
import org.jboss.netty.handler.timeout.{IdleStateEvent, IdleStateAwareChannelHandler}
import org.jboss.netty.util.{HashedWheelTimer, Timeout, TimerTask}
import starling.utils.{StackTraceToString, NamedThreadFactory, Log}
import starling.auth.{User, Client}

case class StateChangeEvent(previous: State, current: State) extends Event

class BouncyRMIClient[C](host: String, port: Int, interface: Class[C], auth: Client, logger:(String)=>Unit=(x)=>{}, overriddenUser:Option[String] = None) {
  private val client = new Client(overriddenUser)
  lazy val clientTimer = new HashedWheelTimer

  def start: Future[Option[scala.Throwable]] = {
    client.init
    client.connect
  }

  override def toString = "BouncyRMIClient:" + client.state.toString

  def startBlocking = {
    try {
      start.get match {
        case Some(t) => throw t
        case None =>
      }
    } catch {
      case e: ExecutionException => throw e.getCause
    }
  }

  def stop = client.stop

  private class Client(overriddenUser:Option[String]) {
    val connectExecutor = Executors.newSingleThreadExecutor(NamedThreadFactory("Client.connect"))

    val stateLock = new Object
    var s: State = stateLock.synchronized {new NotConnected}
    def state = stateLock.synchronized {
      s
    }

    val waitingFor = new java.util.concurrent.ConcurrentHashMap[Int, Waiting]()
    val sequence = new AtomicInteger(0)
    @volatile var channel: Option[Channel] = None
    val bootstrap = new ClientBootstrap(
      new NioClientSocketChannelFactory(
        Executors.newCachedThreadPool(new NamedDaemonThreadFactory("ClientBoss")),
        Executors.newCachedThreadPool(new NamedDaemonThreadFactory("ClientWorker"))))

    private def stateTransition(event: Any) = stateLock.synchronized {
      val previous = s
      s = s.move(event)
      publisher.publish(StateChangeEvent(previous, s))
      s
    }

    def init = {
      bootstrap.setPipelineFactory(new ClientPipelineFactory(new ClientHandler, clientTimer, logger))
      bootstrap.setOption("keepAlive", true)
      bootstrap.setOption("remoteAddress", new InetSocketAddress(host, port))
    }

    def connectBlocking = {
      connect.get match {
        case Some(t) => throw t
        case None =>
      }
    }

    def connect = {
      val call = new Callable[Option[Throwable]] {
        def call = {
          val semaphore = new Semaphore(0, true)
          val future = bootstrap.connect
          val channelFuture = future.awaitUninterruptibly

          var result: Option[Throwable] = None
          if (!channelFuture.isSuccess) {
            Log.warn("Client: Failed to connect to server")
            result = Some(new CannotConnectException("Can not connect to server", channelFuture.getCause))
            semaphore.release
          } else {
            val ch = channelFuture.getChannel
            val sslHandler = ch.getPipeline.get(classOf[SslHandler])
            // Get notified when SSL handshake is done.
            val handshakeFuture = sslHandler.handshake.awaitUninterruptibly
            if (!handshakeFuture.isSuccess) {
              handshakeFuture.getCause match {
                case she: SSLHandshakeException => result = Some(new CannotConnectException("SSL failed to connect", she.getCause.getCause))
                case e => result = Some(new CannotConnectException("SSL failed to connect, unrecognised reason", e))
              }
              Log.warn("Client: SSL handshake failed")
              semaphore.release
            } else {
              Log.info("Client: SSL connected")
              channel = Some(ch)

              reactions += {
                case StateChangeEvent(_, _: ConnectingState) =>
                case StateChangeEvent(f, ClientConnected) => {
                  semaphore.release
                }
                case StateChangeEvent(ServerDisconnected(msg), _) => {
                  //Preserve shutdown message if reconnect fails
                  result = Some(new OfflineException(msg))
                }
                case StateChangeEvent(f, state) => {
                  result = state match {
                    case ClientConnecting => Some(new OfflineException("Not connected. Connecting"))
                    case Reconnecting(t) => Some(new OfflineException("Unexpected disconnect, in the process of reconnecting", t))
                    case ClientDisconnected => Some(new OfflineException("Client disconnected, can't reuse"))
                    case ServerDisconnected(msg) => Some(new OfflineException(msg))
                    case ServerUpgrade(v) => Some(new ServerUpgradeException(v))
                    case AuthFailed => Some(new AuthFailedException("Authorisation failed against server"))
                    case _:NotConnected => Some(new OfflineException("Not connected."))
                  }
                  semaphore.release
                }
              }
              stateTransition(ClientConnectedEvent)
              Log.info("Client: Send Auth")
              ch.write(AuthMessage(auth.ticket, overriddenUser))
            }
          }
          semaphore.acquire
          result
        }
      }
      connectExecutor.submit(call)
    }

    def reconnect {
      Log.info("Starting reconnect")

      clientTimer.newTimeout(new TimerTask() {
        var delay = 100

        def run(timeout: Timeout) = {
          try {
            state match {
              case ClientDisconnected =>
              case _ => {
                delay = delay * 2
                connectBlocking
              }
            }
          }
          catch {
            case e: CannotConnectException => {
              clientTimer.newTimeout(this, scala.math.min(60 * 1000, delay), TimeUnit.MILLISECONDS)
            }
            case _ => // don't reschedule for any other type of exception. just stay disconnected
          }
        }
      }, 100, TimeUnit.MILLISECONDS)
    }

    def invokeMethod(method: Method, args: Array[Object], onException: (Throwable => Object)): Object = {
      val id = sequence.getAndIncrement
      val methodRequest = new MethodInvocationRequest(
        BouncyRMI.CodeVersion,
        id,
        method.getName,
        method.getParameterTypes.map(_.getName),
        if (args == null) new Array(0) else args)

      //try to reconnect if offline
      try {
        state match {
          case _:ServerDisconnected | _:ConnectFailed => connectBlocking
          case _ =>
        }
      } catch {
        case e:CannotConnectException => 
      }

      val waiting = new Waiting
      waitingFor.put(id, waiting)
      try {
        state match {
          case ClientConnected => {
            (channel: @unchecked) match {
              case Some(ch) => {
                val result = ch.write(methodRequest)
                result.addListener(new ChannelFutureListener {
                  def operationComplete(future: ChannelFuture) = {
                    if (!future.isSuccess) {
                      waiting.exception(future.getCause)
                    }
                  }
                })
                waiting.waitAndReturn
              }
            }
          }
          case ClientConnecting => onException(new OfflineException("Not connected. Connecting"))
          case Reconnecting(t) => onException(new OfflineException("Unexpected disconnect, in the process of reconnecting", t))
          case ClientDisconnected => onException(new OfflineException("Client disconnected, can't reuse"))
          case ServerDisconnected(msg) => onException(new OfflineException(msg))
          case ServerUpgrade(v) => onException(new ServerUpgradeException(v))
          case other:NotConnected => onException(new OfflineException("Not connected."))
        }
      } catch {
        case e: IllegalStateException => {
          handleException(e, channel)
          onException(e)
        }
      } finally {
        waitingFor.remove(id)
      }
    }

    private val closeMonitor = new AtomicBoolean(false)

    private def close = {
      val hasAlreadyBeenClosed = closeMonitor.getAndSet(true)
      if (hasAlreadyBeenClosed) {
        Log.warn("The client has already been closed so won't do anything here")
      } else {
        Log.info("Completely Closing Client")
        channel match {
          case Some(ch) => {
            channel = None
            ch.disconnect().await(3, TimeUnit.SECONDS)
            ch.close().await(3, TimeUnit.SECONDS)
            ch.unbind().await(3, TimeUnit.SECONDS)
          }
          case None =>
        }
        connectExecutor.shutdown
        clientTimer.stop

        bootstrap.releaseExternalResources
        publisher.shutdown
      }
    }

    def killAllWaitingFors(t: Throwable) {
      for (wait <- waitingFor.values.toArray(Array[Waiting]())) {
        wait.exception(t)
      }
    }

    def stop = {
      stateTransition(ClientDisconnectedEvent)
      close
    }

    def handleException(cause: Throwable, channel: Option[Channel]) {
      killAllWaitingFors(cause)
      channel match {
        case Some(ch) => ch.close
        case None =>
      }
      state match {
        case ClientConnected => {reconnect}
        case _ =>
      }
      stateTransition(UnexpectedDisconnectEvent(cause))
    }

    class ClientHandler extends IdleStateAwareChannelHandler {

      override def channelConnected(ctx: ChannelHandlerContext, e: ChannelStateEvent) {
        Log.info("Client: Channel connected.")        
      }

      override def channelDisconnected(ctx: ChannelHandlerContext, e: ChannelStateEvent) {
        val cause = new Exception("Disconnected")
        killAllWaitingFors(cause)
        state match {
          case ClientConnected => {
            stateTransition(ShutdownMessage("Server shutdown"))
            reconnect
          }
          case _ =>
        }
        Log.info("Client: Channel disconnected")
      }

      override def exceptionCaught(ctx: ChannelHandlerContext, e: ExceptionEvent) {
        val cause = e.getCause();
        handleException(cause, Some(ctx.getChannel()))
      }

      override def messageReceived(ctx: ChannelHandlerContext, e: MessageEvent) {
        val message = e.getMessage
        message match {
          case AuthSuccessfulMessage => {
            Log.info("Client: Auth successful, sending version check")
            e.getChannel.write(VersionCheckRequest)
          }
          case v: VersionCheckResponse => {
            stateTransition(v) match {
              case s: ServerUpgrade => {
                new Thread("Client closer thread") {
                  override def run = {close}
                }.start
              }
              case _ => // all good
            }
          }
          case BroadcastMessage(event) => publisher.publish(event)
          case s: ShutdownMessage => {
            stateTransition(s)
            channel match {
              case Some(ch) => {
                channel = None
                ch.close
                reconnect
              }
              case None =>
            }
          }
          case StdOutMessage(_, line) => println("S: " + line)
          case ServerException(t: Throwable) => killAllWaitingFors(t)
          case MethodInvocationResult(id, result) => waitingFor.get(id).setResult(result)
          case MethodInvocationException(id, t) => waitingFor.get(id).exception(t)
          case MethodInvocationBadVersion(id, serverVersion) => {
            channel match {
              case Some(ch) if serverVersion != BouncyRMI.CodeVersion => {
                channel = None
                ch.close
              }
              case _ =>
            }
            stateTransition(VersionCheckResponse(serverVersion))
          }
          case AuthFailedMessage => {
            stateTransition(AuthFailedMessage)
            stop
          }
          case PongMessage =>
          case m => {
            Log.error("Unrecognised message, disconnecting: " + m)
            stop
          }
        }
      }

      override def channelIdle(ctx: ChannelHandlerContext, e: IdleStateEvent) = {
        state match {
          case ClientConnected => {
            e.getChannel().write(PingMessage)
          }
          case _ =>
        }
      }
    }
  }

  val proxy: C = {
    val e = new Enhancer()
    e.setSuperclass(interface)
    e.setCallback(new MethodInterceptor() {
      def intercept(obj: Object, method: Method,
                    args: Array[Object], proxy: MethodProxy): Object = {
        if (method.getName == "finalize") {
          null
        } else {
          client.invokeMethod(method, args, (e) => {
            throw e
          })
        }
      }
    });
    e.create().asInstanceOf[C]
  }

  private val publisher = new Object() {
    val executor = Executors.newSingleThreadScheduledExecutor()
    val publisher = new scala.swing.Publisher() {}

    def publish(event: Event) {
      try {
        executor.submit(new Runnable() {
          def run() {
            publisher.publish(event)
          }
        })
      } catch {
        case e => Log.warn("Asked to publish but publisher has been closed")
      }
    }

    def reactions = publisher.reactions

    def shutdown() {
      executor.shutdown()
    }
  }

  val reactions = publisher.reactions
  val remotePublisher = publisher.publisher

  class Waiting {
    private val lock = new Object
    private var completed = false
    private var result: AnyRef = null
    private var exception: Throwable = null
    private var serverVersion: String = null

    def waitAndReturn = {
      lock.synchronized {
        if (!completed) {
          lock.wait()
        }
        if (exception != null) {
          throw exception
        }
        if (serverVersion != null) {
          throw new ServerUpgradeException(serverVersion)
        }
        result
      }
    }

    def exception(e: Throwable) {
      lock.synchronized {
        this.completed = true
        exception = e
        lock.notify
      }
    }

    def badVersion(serverVersion: String) {
      lock.synchronized {
        this.completed = true
        this.serverVersion = serverVersion
        lock.notify
      }
    }

    def setResult(result: Object) = {
      lock.synchronized {
        this.completed = true
        this.result = result
        lock.notify
      }
    }
  }

}
