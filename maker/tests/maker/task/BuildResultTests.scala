package maker.task

import org.scalatest.FunSuite
import maker.utils.FileUtils._
import maker.project.Module
import tasks.CleanTask
import maker.utils.Stopwatch
import maker.task.compile._
import maker.task.tasks.CleanTask
import maker.ScalaVersion



/**
 * tests basic composition of build results through for comprehension
 */
class BuildResultTests extends FunSuite {

  private val SCALA_VERSION = ScalaVersion.TWO_ELEVEN_DEFAULT

  case class MyDontRunTask(baseProject : Module) extends Task {
    def name = "Don't run this task"
    def upstreamTasks = Nil
    def copy_(p : Module) = copy(baseProject = p)
    def exec(results : Iterable[TaskResult], sw : Stopwatch) = {
      assert(false, "should not run task!")
      DefaultTaskResult(this, true, sw)
    }
  }
  test("buildResultsShouldCompose") {
    withTempDir { root =>
      val emptyGraph = Dependency.Graph.empty

      val p1 = new Module(file(root, "p1"), "p1")
      val p2 = new Module(file(root, "p2"), "p2")
      val p3 = new Module(file(root, "p3"), "p3")

      import TaskResult._

      val sw = new Stopwatch()

      // some success build results
      val br1 = BuildResult("foo", 
        List(
          DefaultTaskResult(CleanTask(p1), true, sw), 
          DefaultTaskResult(CleanTask(p3), true, sw)), emptyGraph)
      val br2 = BuildResult("foo", List(DefaultTaskResult(CompileTask(p2, p2, SourceCompilePhase), true, sw)), emptyGraph)

      // and some failures
      val fr1 = BuildResult("foo", List(DefaultTaskResult(CompileTask(p2, p2, SourceCompilePhase), false, sw, message = Some("was broke"))), emptyGraph)

      val r1 = for {
        w <- br1
        x <- br1
        y <- br2
        z <- br2
      } yield z

      assert(r1.results.size == 6, "task results should concatenate")
      assert(r1.succeeded == true, "build result of successes should yield success")

      // todo, assert build results are concatenated properly into the final build result

      // assert that a task is run if it's successful so far
      try {
        for {
          x <- br1
          y <- br2
          z <- BuildResult("foo", List(MyDontRunTask(p1).exec(y.results, Stopwatch())), emptyGraph)
        } yield z
        assert(false, "should have run task and didnt")
      }
      catch {
        case _ : Throwable =>
      }

      // assert that subsequent tasks do not run if an earlier task failed and that final outcome is a failure
      val r2 = for {
        _ <- br1
        _ <- fr1
        y <- br1
        z <- BuildResult("foo", List(MyDontRunTask(p1).exec(y.results, Stopwatch())), emptyGraph)
      } yield z

      assert(r2.results.size == 3, "failed task results should not concatenate")
      assert(r2.succeeded == false, "build result of successes with fail should yield failure")
    }
  }
}
