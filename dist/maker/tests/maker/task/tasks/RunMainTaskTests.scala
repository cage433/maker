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

package maker.task.tasks

import org.scalatest.FunSuite
import maker.utils.FileUtils._
import maker.utils.RichString._
import maker.project.{TestProject, Project}

class RunMainTaskTests extends FunSuite {

  /**
   * Check run main works, and runs the main only on the project it is invoked on,
   *   but still runs dependency tasks in upstream projects and within the target project correctly
   */
  test("Can run task run-main with correct upstream tasks run first") {
    withTempDir{
      dir ⇒
        val p1Dir = file(dir, "p1")
        val p2Dir = file(dir, "p2")

        val proj1 = new TestProject(p1Dir, "p1")
        val proj2 = new TestProject(p2Dir, "p2", upstreamProjects = List(proj1))

        val outputFile = file(p1Dir, "output.txt")
        assert(! outputFile.exists)

        proj1.writeSrc(
          "bar/Bar.scala",
          """
            package bar

            object Bar extends {
              val x = 10
            }
          """
        )

        proj2.writeSrc(
          "foo/Main.scala",
          """
            package foo

            import java.io._
            import bar.Bar

            object Main extends App{

              val file = new File("%s")
              val fstream = new FileWriter(file)
              val out = new BufferedWriter(fstream)
              out.write("Hello " + Bar.x)
            }
          """ % outputFile.getAbsolutePath
        )

        // this should compile proj1 before compiling and then running the main in proj2
        proj2.runMain("foo.Main")()()

        assert(outputFile.exists)
    }
  }
}
