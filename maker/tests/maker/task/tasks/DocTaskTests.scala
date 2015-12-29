package maker.task.tasks

import org.scalatest.{Matchers, FreeSpec}
import maker.TestMakerRepl
import maker.utils.FileUtils

class DocTaskTests 
  extends FreeSpec with Matchers
  with FileUtils
{
  "Doc should be produced " in {
    withTempDir{
      rootDirectory => 
        writeToFile(
          file(rootDirectory, "src/foo/Foo.scala"),
          """
            package foo

            case class Foo(n : Int)
          """
        )
        TestMakerRepl.writeProjectFile(
          rootDirectory,
          s"""
            lazy val a = new Module(
              root = file("$rootDirectory"),
              name = "a"
            ) with ClassicLayout 

            lazy val p = new Project(
              "p",
              file("$rootDirectory"),
              Seq(a)
            ) 
          """
        )
        val repl = TestMakerRepl(rootDirectory)
        val indexHtmlFile = file(rootDirectory, "docs/2.11.6/index.html")
        indexHtmlFile.exists should be (false)
        repl.inputLine("p.doc")
        indexHtmlFile.exists should be (true)
    }
  }
}
