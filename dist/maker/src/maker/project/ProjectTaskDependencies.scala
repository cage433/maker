package maker.project
import maker.task.Task
import maker.task.Dependency
import maker.task.BuildResult
import maker.task.compile.CompileTask
import maker.utils.FileUtils._
import java.io.BufferedWriter

trait ProjectTaskDependencies{
  self : Project ⇒ 

    def extraUpstreamTasks(task : Task) : Set[Task] = Set.empty
    def extraDownstreamTasks(task : Task) : Set[Task] = Set.empty

    // Run exactly once BEFORE this task is called from a project - NOT once for
    // each task in the dependency tree
    def setUp(graph : Dependency.Graph){}
    def tearDown(graph : Dependency.Graph, result : BuildResult){
      props.VimErrorFile().delete

      result.results.map(_.task).foreach{
        case t : CompileTask ⇒ 
          withFileAppender(props.VimErrorFile()){
            writer : BufferedWriter ⇒ 
              t.projectPhase.vimCompileOutputFile.readLines.foreach(writer.println)
          }
        case _ ⇒ 
      }
    }
}
