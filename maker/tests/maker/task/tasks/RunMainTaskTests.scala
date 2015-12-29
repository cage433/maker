package maker.task.tasks

import org.scalatest.FunSuite
import maker.utils.FileUtils._
import maker.utils.RichString._
import maker.utils.FileUtils
import maker.TestMakerRepl

class RunMainTaskTests extends FunSuite with FileUtils {

  /**
   * Check run main works, and runs the main only on the module it is invoked on,
   *   but still runs dependency tasks in upstream modules and within the target module correctly
   */
  test("Can run task run-main with correct upstream tasks run first") {
    withTempDir{
      rootDirectory =>

        val outputFile = file(rootDirectory, "output.txt")
        assert(! outputFile.exists)

        writeToFile(
          file(rootDirectory, "a/src/bar/Bar.scala"),
          """
            package bar

            object Bar extends {
              val x = 10
            }
          """
        )

        writeToFile(
          file(rootDirectory, "b/src/foo/Main.scala"),
          s"""
            package foo

            import java.io._
            import bar.Bar

            object Main extends App{

              val file = new File("${outputFile.getAbsolutePath}")
              val fstream = new FileWriter(file)
              val out = new BufferedWriter(fstream)
              out.write("Hello " + Bar.x)
            }
          """ 
        )

        TestMakerRepl.writeProjectFile(
          rootDirectory,
          s"""
            lazy val a = new Module(
              root = file("$rootDirectory", "a"),
              name = "a"
            ) with ClassicLayout 

            lazy val b = new Module(
              root = file("$rootDirectory", "b"),
              name = "b",
              compileDependencies = Seq(a)
            ) with ClassicLayout 

            lazy val p = new Project(
              "p",
              file("$rootDirectory"),
              Seq(b)
            ) 
          """
        )

        val repl = TestMakerRepl(rootDirectory)
        repl.inputLine("""p.runMain("foo.Main")()()""")

        assert(outputFile.exists)
    }
  }
}
