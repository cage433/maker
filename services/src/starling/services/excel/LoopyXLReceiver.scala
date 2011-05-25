package starling.services.excel

import starling.loopyxl._
import java.net.{Socket, ServerSocket}
import java.util.concurrent.atomic.AtomicInteger
import starling.bouncyrmi.AuthHandler
import starling.auth.User
import starling.utils._

import starling.utils.ImplicitConversions._
import LoopyXL._
import MessageType._
import ClosureUtil._


class LoopyXLReceiver(port : Int, authHandler: AuthHandler, owners: AnyRef*) extends StoppableThread(isRunning =>
  Log.infoF("LoopyXLReceiver started listening on port: " + port) {
    val id = new AtomicInteger();
    val methodSource = new ReflectiveMethodSource(owners : _*)
    val serverSocket = decorate("Could not bind to port: " + port) {new ServerSocket(port)}

    while (isRunning()) safely {
      serverSocket.accept.update { socket => startDaemon(() => handle(socket)) }
    }.printStackTrace

    def handle(socket: Socket): Unit = safely { using(socket) { s =>
      var authenticatedUser : Option[User] = None
      val handlers = new LoopyXLHandlers(
        AUTHENTICATE → new AuthenticationHandler(id, authHandler.withCallback(authenticatedUser = _)),
        LOOKUP       → new LookupHandler(id, methodSource),
        INVOCATION   → new InvocationHandler(id, methodSource, User.pushUser(authenticatedUser)))
      val (inputStream, outputStream) = (s.getInputStream, s.getOutputStream)

      while (isRunning()) {
        val request = Request.parseDelimitedFrom(inputStream)

        if (request != null) {
          handlers.handle(request).update { response => response.writeDelimitedTo(outputStream) }
        }
      }
    } }.printStackTrace
  }
)
