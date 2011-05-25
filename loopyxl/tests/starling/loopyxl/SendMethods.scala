package starling.loopyxl

import java.io.IOException
import java.io.InputStream
import java.net.ServerSocket
import java.net.Socket
import starling.utils.ClosureUtil
import LoopyXL.MessageType._
import starling.bouncyrmi.AuthHandler
import java.util.concurrent.atomic.AtomicInteger

object SendMethods extends Application {
  private val methodSource = new ReflectiveMethodSource(new MethodImplementation)

  private val loggingHandler = new AuthHandler {
    def authorized(ticket: Option[Array[Byte]]) = {
      println("authorized: " + ticket)
      None
    }
  }

  private val id = new AtomicInteger(1)
  private val handlers = Map(
    AUTHENTICATE → new AuthenticationHandler(id, loggingHandler),
    LOOKUP       → new LookupHandler(id, methodSource),
    INVOCATION   → new InvocationHandler(id, methodSource, ClosureUtil.Null)
  )

  val serverSocket: ServerSocket = new ServerSocket(5169)
  while (true) {
    try {
      val socket: Socket = serverSocket.accept
      new Thread { _ => Handle(socket) }.start
    }
    catch {
      case e: Exception => {
        e.printStackTrace
      }
    }
  }

  private def Handle(socket: Socket): Unit = {
    try {
      val inputStream = socket.getInputStream
      val outputStream = socket.getOutputStream
      while (true) {
        val request = LoopyXL.Request.parseDelimitedFrom(inputStream)
        val option: Option[Handler] = handlers.get(request.getType)
        val response = option.get.handle(request)

        response.writeDelimitedTo(outputStream)
      }
    }
    catch {
      case e: Exception => {
        e.printStackTrace
      }
    }
    finally {
      if (socket != null) {
        try {
          socket.close
        }
        catch {
          case e: IOException => {
            e.printStackTrace
          }
        }
      }
    }
  }

  private def parseLookupRequest(inputStream: InputStream): LoopyXL.LookupRequest = {
    val request = LoopyXL.Request.parseDelimitedFrom(inputStream)
    if (request.getType != LoopyXL.MessageType.LOOKUP) {
      throw new IOException("Invalid response")
    }
    request.getLookup
  }
}
