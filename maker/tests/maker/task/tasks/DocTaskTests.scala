package maker.task.tasks

import org.scalatest.{Matchers, FreeSpec}
import maker.utils.FileUtils._
import maker.project.{TestModule, Project}

class DocTaskTests extends FreeSpec with Matchers{
  "Doc should be produced " in {
    withTempDir{
      dir => 
        val module = new TestModule(dir, "DocTaskTests")
        val proj = Project("DocTaskTests", dir, module :: Nil, isTestProject = true)
        val indexHtmlFile = file(proj.docOutputDir, "index.html")
        indexHtmlFile.exists should be (false)
        module.writeSrc(
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
