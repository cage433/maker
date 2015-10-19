package maker.task.compile

import org.scalatest.{Matchers, FreeSpec}
import maker.utils.FileUtils
import maker.project.TestModule

class CrossCompilationTests extends FreeSpec with Matchers{
  import FileUtils.{withTempDir, writeToFile, file}
  "Can compile a scala 2.9 project" in {
    withTempDir{
      dir => 
        writeToFile(
          file(dir, "external-resource-config"),
          """|resolver: default http://repo.typesafe.com/typesafe/releases/
             |version: scala_version 2.9.3""".stripMargin
        )
        val module = new TestModule(dir, "CrossCompilationTests")
        module.writeCaseObject("Foo", "foo")
        List("scala-library", "scala-compiler").foreach{
          name => 
            // TODO - better
        }
        val res = module.compile
        res.succeeded should be (true)
          
    }
  }
}
