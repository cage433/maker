package maker.project.extras

import maker.project.ProjectTaskDependencies
import maker.utils.os.Command
import maker.utils.os.CommandOutputHandler
import maker.project.Project
import maker.task.Task
import maker.task.BuildResult
import maker.task.Dependency
import maker.utils.RichString._

trait TmuxMessaging extends ProjectTaskDependencies{
  self : Project ⇒ 
  def tmux(args : String*){
    val hasTmux = (Command(log, CommandOutputHandler.NULL, None, "which", "tmux").withNoOutput.exec == 0)
    if (hasTmux)
      new Command(log, CommandOutputHandler.NULL, None, ("tmux"::args.toList) : _*).withNoOutput.execAsync
  }
  tmux("set-option", "-gq", "status-left-length", "100")

  def tmuxReportTaskFailed(msg : String){
    tmux("set-option", "-gq", "status-left", "#[bg=red,fg=black] %s" % msg)
  }

  def tmuxClearStatusLeft{
    tmux("set-option", "-gq", "status-left", "")
  }

  override def setUp(graph : Dependency.Graph){
    tmux("refresh-client")
    tmux("set", "-g", "status-bg", "blue")
    super.setUp(graph)
  }
  override def tearDown(graph : Dependency.Graph, result : BuildResult){
    tmux("set", "-g", "status-bg", "default")
    if (result.succeeded){
      tmuxClearStatusLeft
    } else {
      tmuxReportTaskFailed(result.originalTask + " failed ")
    }
    super.tearDown(graph, result)
  }
}
