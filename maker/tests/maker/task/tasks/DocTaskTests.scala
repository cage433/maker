package maker.task.tasks

import org.scalatest.{Matchers, FreeSpec}
import maker.utils.FileUtils._
import maker.project.TestModule

class DocTaskTests extends FreeSpec with Matchers{
  "Doc should be produced " in {
    withTempDir{
      dir => 
        val proj = new TestModule(dir, "DocTaskTests")
        val indexHtmlFile = file(proj.rootAbsoluteFile, "target-maker", "docs", "index.html")
        indexHtmlFile.exists should be (false)
        proj.writeSrc(
          "foo/Foo.scala",
          """
            package foo

            case class Foo(n : Int)
          """
        )
        proj.doc
        indexHtmlFile.exists should be (true)
    }
  }
}
