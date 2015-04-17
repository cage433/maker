package maker.project

import maker.task.{Dependency, BuildResult}
import maker.task.compile.CompileTask
import maker.utils.FileUtils._
import java.io.BufferedWriter
import maker.utils.RichString._
import maker.utils.os.Command
import maker.task.tasks.RunUnitTestsTask

trait TmuxIntegration extends BaseProject{

    private lazy val hasTmux = Command("which", "tmux").withNoOutput.run == 0
    def tmux(args : String*){
      if (!isTestProject && hasTmux)
        Command(("tmux"::args.toList) : _*).withNoOutput.runAsync
    }

    private def tmuxReportTaskFailed(msg : String){
      tmux("set-option", "-gq", "status-left", "#[bg=red,fg=black] %s" % msg)
    }

    private def tmuxReportTestFailed(msg : String){
      tmux("set-option", "-gq", "status-left", "#[bg=yellow,fg=black] %s" % msg)
    }
    private def tmuxClearStatusLeft{
      tmux("set-option", "-gq", "status-left", "")
    }

    // Run exactly once BEFORE this task is called from a module - NOT once for
    // each task in the dependency tree
    override def setUp(graph : Dependency.Graph) = {
      val superResult = super.setUp(graph)
      tmux("set-option", "-gq", "status-left-length", "100")
      tmux("refresh-client")
      tmuxClearStatusLeft
      tmux("set", "-g", "status-bg", "blue")
      superResult
    }

    private def tmuxReportResult(result : BuildResult){
      tmux("set", "-g", "status-bg", "default")
      result.maybeFirstFailure match {
        case Some(failingTaskResult) => 
          if (failingTaskResult.isTestResult)
            tmuxReportTestFailed(s"$name failed")
          else
            tmuxReportTaskFailed(s"${result.name} failed ")
        case None =>
      }
    }

    def tearDown(graph : Dependency.Graph, result : BuildResult) = {
      tmuxReportResult(result)
      true
    }
}
