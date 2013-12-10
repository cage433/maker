package maker.project
import maker.task.Task
import maker.build.Dependency
import maker.build.BuildResult
import maker.task.compile.CompileTask
import maker.utils.FileUtils._
import java.io.BufferedWriter
import maker.utils.Implicits.RichString._
import maker.utils.os.Command
import maker.utils.os.CommandOutputHandler
import maker.build.Dependency
import maker.build.BuildManager.TimedResults

trait TmuxIntegration{
  self : BaseProject => 

    private lazy val hasTmux = (Command(CommandOutputHandler.NULL, None, "which", "tmux").withNoOutput.exec == 0)
    def tmux(args : String*){
      if (props.TmuxMessaging() && hasTmux)
        new Command(CommandOutputHandler.NULL, None, ("tmux"::args.toList) : _*).withNoOutput.execAsync
    }

    private def tmuxReportTaskFailed(msg : String){
      tmux("set-option", "-gq", "status-left", "#[bg=red,fg=black] %s" % msg)
    }

    private def tmuxClearStatusLeft{
      tmux("set-option", "-gq", "status-left", "")
    }

    // Run exactly once BEFORE this task is called from a module - NOT once for
    // each task in the dependency tree
    def setUp(graph : Dependency.Graph){
      tmux("set-option", "-gq", "status-left-length", "100")
      tmux("refresh-client")
      tmuxClearStatusLeft
      tmux("set", "-g", "status-bg", "blue")
    }

    private def outputCompilationToVimErrorFile(result : TimedResults){
      props.VimErrorFile().delete
      result.results.map(_.task).foreach{
        case t : CompileTask => 
          withFileAppender(props.VimErrorFile()){
            writer : BufferedWriter => 
              t.modulePhase.vimCompileOutputFile.readLines.foreach(writer.println)
          }
        case _ => 
      }
    }

    private def tmuxReportResult(result : TimedResults){
      tmux("set", "-g", "status-bg", "black")
      if (result.failed){
        tmuxReportTaskFailed("task failed ")
      }
    }

    def tearDown(graph : Dependency.Graph, result : TimedResults){
      tmuxReportResult(result)
      outputCompilationToVimErrorFile(result)
    }
}
