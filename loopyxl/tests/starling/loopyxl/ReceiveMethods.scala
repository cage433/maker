package starling.loopyxl

import java.io.IOException
import java.io.InputStream
import java.net.ServerSocket
import java.net.Socket

object ReceiveMethods {
  def main(args: Array[String]): Unit = {
    val socket: Socket = new Socket("localhost", 9876)
    val methodsRequest: LoopyXL.LookupRequest = LoopyXL.LookupRequest.newBuilder.build

    methodsRequest.writeDelimitedTo(socket.getOutputStream)

    val inputStream: InputStream = socket.getInputStream
    val methodsResponse: LoopyXL.LookupResponse = LoopyXL.LookupResponse.parseDelimitedFrom(inputStream)

    socket.close
  }
}


