package maker.remote

import java.io.{BufferedReader, InputStreamReader, PrintWriter}
import java.net.Socket
import maker.utils.os.Command
import maker.utils.Int

case class RemoteBuild(remoteHost : String, sshPort : Int, socketPort : Int, remoteAbsDir : String){
  def remoteCompile(){
    val copyScalaFilesCommand = Command(
      "rsync", 
      "-aive", 
      s"'ssh -p $sshPort'", 
      "--exclude=.git", 
      "--include='*/'", 
      "--exclude='target*/'", 
      "--include='*.scala'",
      "--include='*.java'",
      "--exclude='*'", 
      "'.'",
      s"$remoteHost:$remoteAbsDir"
    )

    val copyClassFilesCommand = Command(
      "rsync", 
      "-aive", 
      s"'ssh -p $sshPort'", 
      "--exclude=.git", 
      "--include='*/'", 
      "--exclude=src/", 
      "--exclude=tests/", 
      "--include='*.class'",
      "--exclude='*'", 
      s"$remoteHost:$remoteAbsDir",
      "'.'"
    )

    println("Opening socket")
    val socket = new Socket("localhost", socketPort)
    println("Opened socket")
    val in = new BufferedReader(new InputStreamReader(socket.getInputStream()))
    val out = new PrintWriter(socket.getOutputStream(), true)

    val succeeded = try {
      println(s"Copying source files\n$copyScalaFilesCommand")
      if (copyScalaFilesCommand.run() != 0)
        throw new RuntimeException("Failed to copy source files")

      out.println("COMPILE")
      in.readLine() match {
        case "SUCCEEDED" =>
          println("SUCCEEDED")
        case "FAILED" =>
          throw new RuntimeException("Compilation failed")
      }
      if (copyClassFilesCommand.run() != 0)
        throw new RuntimeException("Failed to copy class files")
      true
    } catch {
      case e : Exception =>
        println(e.getMessage)
        false
    }
  }
}


