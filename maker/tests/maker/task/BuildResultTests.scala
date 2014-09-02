/*
 * Copyright (c) 2011-2012, Alex McGuire, Louis Botterill
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package maker.task

import org.scalatest.FunSuite

import maker.utils.FileUtils._
import maker.project.Module
import tasks.CleanTask
import maker.utils.Stopwatch
import maker.project.TestModule
import maker.task.compile._


/**
 * tests basic composition of build results through for comprehension
 */
class BuildResultTests extends FunSuite {

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

      val p1 = new TestModule(file(root, "p1"), "p1")
      val p2 = new TestModule(file(root, "p2"), "p2")
      val p3 = new TestModule(file(root, "p3"), "p3")

      import TaskResult._

      val sw = new Stopwatch()

      // some success build results
      val br1 = BuildResult("foo", 
        List(
          DefaultTaskResult(CleanTask(p1), true, sw), 
          DefaultTaskResult(CleanTask(p3), true, sw)), emptyGraph)
      val br2 = BuildResult("foo", List(DefaultTaskResult(SourceCompileTask(p2), true, sw)), emptyGraph)

      // and some failures
      val fr1 = BuildResult("foo", List(DefaultTaskResult(SourceCompileTask(p2), false, sw, message = Some("was broke"))), emptyGraph)

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
