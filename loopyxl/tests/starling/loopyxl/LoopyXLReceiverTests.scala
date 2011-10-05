package starling.loopyxl

import util.Random
import starling.utils.ClosureUtil._
import starling.utils.ImplicitConversions._
import java.net.Socket
import java.io.InputStream
import starling.loopyxl.LoopyXL.MessageType
import scala.collection.JavaConversions._
import starling.auth.{AuthHandler}

object LoopyXLReceiverTests {
  def main(args: Array[String]) {
    // Should accept multiple connections

    val port = 1024 + ((Random.nextInt() % 6400) * 10).abs + 5

    using (new LoopyXLReceiver(port, AuthHandler.Never, new ReflectiveMethodSource(new MethodImplementation))) { receiver => {
      receiver.start

      Range(1, 250).map(i => receiveMethods(port)).map(_.close)
    } }
  }

  def receiveMethods(port: Int): Socket = {
    val socket: Socket = new Socket("localhost", port)
    val methodsRequest: LoopyXL.Request = LoopyXL.Request.newBuilder.setId(1).setType(MessageType.LOOKUP).update { request =>
      request.setLookup(LoopyXL.LookupRequest.newBuilder.build)
    }.build

    methodsRequest.writeDelimitedTo(socket.getOutputStream)

    val inputStream: InputStream = socket.getInputStream
    val methodsResponse: LoopyXL.Response = LoopyXL.Response.parseDelimitedFrom(inputStream)
    methodsResponse.getLookup.getDefinitionsList.foreach(md => println(md.getName))

    socket
  }
}