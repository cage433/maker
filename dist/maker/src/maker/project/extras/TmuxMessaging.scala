package maker.project.extras

import maker.project.ProjectTaskDependencies
import maker.utils.os.Command
import maker.utils.os.CommandOutputHandler
import maker.project.Project
import maker.task.Task
import maker.task.BuildResult
import maker.task.Dependency

trait TmuxMessaging extends ProjectTaskDependencies{
  self : Project ⇒ 
  def tmux(args : String*){
    val hasTmux = (Command(log, CommandOutputHandler.NULL, None, "which", "tmux").withNoOutput.exec == 0)
    if (hasTmux)
      new Command(log, CommandOutputHandler.NULL, None, ("tmux"::args.toList) : _*).withNoOutput.execAsync
  }
  override def setUp(graph : Dependency.Graph){
    tmux("display-message", "task launched")
    tmux("set", "-g", "status-bg", "magenta")
    super.setUp(graph)
  }
  override def tearDown(graph : Dependency.Graph, result : BuildResult){
    if (result.succeeded){
      tmux("display-message", "task succeeded")
      tmux("set", "-g", "status-bg", "green")
    } else {
      tmux("display-message", "task failed")
      tmux("set", "-g", "status-bg", "red")
    }
    super.tearDown(graph, result)
  }
}
