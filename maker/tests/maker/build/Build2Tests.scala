package maker.build

import org.scalatest.FunSuite
import maker.task.Task
import maker.task.TaskResult
import maker.utils.Stopwatch

class Build2Tests extends FunSuite {
  test("Build one task"){
    var acc : List[Int] = Nil
    def collect(n : Int) = synchronized{
      acc = n :: acc
    }

    case class TestTask(n : Int) extends Task{
      val name = "Test " + n
      def exec(upstreamResults : Iterable[TaskResult], sw : Stopwatch) = {
        collect(n)
        TaskResult.success(this, sw)
      }
      def upstreamTasks = Nil
    }

    val graph = Dependency.Graph(
      Set(TestTask(0))
    )
    val build = new Build2(() => graph)
    build.execute
    assert(acc === List(0))

  }
}
