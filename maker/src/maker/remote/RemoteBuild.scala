package maker.remote

import java.io.{BufferedReader, InputStreamReader, PrintWriter}
import java.net.Socket

object RemoteBuild{
  def remoteCompile(){
    val socket = new Socket("localhost", 5555)
    val in = new BufferedReader(new InputStreamReader(socket.getInputStream()))
    val out = new PrintWriter(socket.getOutputStream(), true)
  }
}


