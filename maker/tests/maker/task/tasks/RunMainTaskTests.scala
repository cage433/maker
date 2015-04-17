package maker.task.tasks

import org.scalatest.FunSuite
import maker.utils.FileUtils._
import maker.utils.RichString._
import maker.project.{TestModule, Module}

class RunMainTaskTests extends FunSuite {

  /**
   * Check run main works, and runs the main only on the module it is invoked on,
   *   but still runs dependency tasks in upstream modules and within the target module correctly
   */
  test("Can run task run-main with correct upstream tasks run first") {
    withTempDir{
      dir =>
        val p1Dir = file(dir, "p1")
        val p2Dir = file(dir, "p2")

        val proj1 = new TestModule(p1Dir, "p1")
        val proj2 = new TestModule(p2Dir, "p2", upstreamProjects = List(proj1))

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
        proj2.compile
        proj2.runMain("foo.Main")()()

        assert(outputFile.exists)
    }
  }
}
