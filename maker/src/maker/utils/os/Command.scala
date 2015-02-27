package maker.utils.os

import java.lang.ProcessBuilder
import java.io.{File, InputStreamReader, BufferedReader, PrintWriter}
import concurrent.Future
import java.io.FileWriter
import scalaz.syntax.std.option._
import java.io.IOException
import maker.utils.FileUtils._
import scala.concurrent.ExecutionContext.Implicits.global

case class CommandOutputHandler(writer : Option[PrintWriter] = Some(new PrintWriter(System.out)),
                                buffer : Option[StringBuffer] = None,
                                closeWriter : Boolean = false) {
  def withSavedOutput = copy(buffer = Some(new StringBuffer()))
  def savedOutput = buffer.cata(_.toString, "")
  def processLine(line : String){
    writer.foreach{
      w =>
        w.println(line)
        w.flush()
    }
    buffer.foreach(_.append(line + "\n"))
  }
  def close() {
    writer.foreach{
      w =>
        w.flush()
        if (closeWriter)
          w.close()
    }
  }

  def redirectOutputRunnable(proc : Process) = new Runnable(){
    def run(){
      val br = new BufferedReader(new InputStreamReader(proc.getInputStream))
      var line : String = null
      def nextLine : String = try {
        br.readLine()
      } catch {
        case _ : IOException => null
      }
      line = nextLine
      while (line != null) {
        processLine(line)
        line = nextLine
      }
      br.close()
      close()
    }
  }
}

object CommandOutputHandler{
  def apply(file : File) : CommandOutputHandler = new CommandOutputHandler(
    writer = Some(new PrintWriter(new FileWriter(file))),
    closeWriter = true
  )
  val NULL = new CommandOutputHandler(writer = None)
  object NO_CONSUME_PROCESS_OUTPUT extends CommandOutputHandler(){
    override def redirectOutputRunnable(proc : Process) = new Runnable(){
      def run(){
      }
    }
  }
}

case class Command(outputHandler : CommandOutputHandler, workingDirectory : Option[File], mergeErrorWithOut : Boolean, args : String*) {

  def savedOutput = outputHandler.savedOutput
  def withOutput(handler : CommandOutputHandler) = new Command(
    outputHandler = handler,
    workingDirectory,
    mergeErrorWithOut,
    args : _*
  )

  def withSavedOutput = withOutput(CommandOutputHandler.NULL.withSavedOutput)
  def withNoOutput = withOutput(CommandOutputHandler.NULL)

  private def startProc() : Process = {
    val procBuilder = new ProcessBuilder(args : _*)
    procBuilder.redirectErrorStream(mergeErrorWithOut)
    workingDirectory.foreach(procBuilder.directory(_))
    procBuilder.start
  }

  private def logCommand() {
    appendToFile(file("maker-commands.log"), this + "\n\n")
  }

  def execAsync() : (Process, Future[Int]) = {
    logCommand()
    val proc = startProc()
    val outputThread = new Thread(outputHandler.redirectOutputRunnable(proc))
    outputThread.start()
    (proc, Future {outputThread.join(); proc.waitFor})
  }

  def exec() : Int = {
    logCommand()
    val proc = startProc()
    outputHandler.redirectOutputRunnable(proc).run()
    proc.waitFor
  }

  def asString = args.mkString(" ")
  override def toString = "Command: " + asString
}

object Command{
  def apply(args : String*) : Command = new Command(CommandOutputHandler(), None, true, args : _*)
  def apply(workingDirectory : Option[File], args : String*) : Command = new Command(CommandOutputHandler(), workingDirectory, true, args : _*)
}

object ScalaCommand {
  def apply(outputHandler : CommandOutputHandler, java : String, opts : List[String], classpath : String, klass : String, name : String, args : List[String] = Nil) : Command = {
    val allArgs : List[String] = java :: opts ::: List[String](
      "-Dscala.usejavacp=true", 
      "-classpath",
      classpath) ::: 
      List("scala.tools.nsc.MainGenericRunner",
      klass) ::: args.toList
    Command(outputHandler, None, true, allArgs :_*)
  }
}

object ScalaDocCmd {
  def apply(outputHandler : CommandOutputHandler, outputDir : File, java : String, classpath : String, opts : List[String], files : File*) : Command = {
    val allArgs : List[String] = List(
      java,
      "-Dscala.usejavacp=true",
      "-classpath",
      classpath) ::: opts :::
      "scala.tools.nsc.ScalaDoc" :: files.map(_.getAbsolutePath).toList
    Command(outputHandler, Some(outputDir), true, allArgs :_*)
  }
  def apply(outputHandler : CommandOutputHandler, outputDir : File, java : String, classpath : String, opts : List[String], optsFile : File) : Command = {
    val allArgs : List[String] = List(
      java,
      "-Dscala.usejavacp=true",
      "-classpath",
      classpath) ::: opts :::
      "scala.tools.nsc.ScalaDoc" :: "@" + optsFile.getAbsolutePath :: Nil
    Command(outputHandler, Some(outputDir), true, allArgs :_*)
  }
}