package maker.remote

import java.io.{BufferedReader, InputStreamReader, PrintWriter}
import java.net.Socket

case class RemoteBuild(remoteHost : String, remotePort : Int, remoteAbsDir : String){
  def remoteCompile(){
    val copyScalaFilesCommand = Command(
      "rsync", 
      "-aive", 
      "ssh -p 5555", 
      "--exclude=.git", 
      "--include=*/", 
      "--exclude=target*/", 
      "--include=*.scala",
      "--exclude=*", 
      "127.0.0.1",
      ":/"home",/"alex",/"repos",/"evcel", "evcel"
    )

    val socket = new Socket("localhost", 5555)
    val in = new BufferedReader(new InputStreamReader(socket.getInputStream()))
    val out = new PrintWriter(socket.getOutputStream(), true)
    out.println("COMPILE")
    in.readLine() match {
      case "SUCCEEDED" =>
        println("SUCCEEDED")
      case "FAILED" =>
        println("FAILED")
    }
  }
}


