package maker.task.compile

import org.scalatest.{Matchers, FreeSpec}
import maker.utils.FileUtils
import maker.project.TestModule

class CrossCompilationTests extends FreeSpec with Matchers{
  import FileUtils.{withTempDir, writeToFile, file, withTestDir}
  "Can compile a scala 2.9 project" in {
    withTestDir{
      dir => 
        writeToFile(
          file(dir, "external-resource-config"),
          """| resolver: default http://repo1.maven.org/maven2/
             | version: scala_version 2.9.3""".stripMargin
        )
        val module = new TestModule(dir, "CrossCompilationTests")
        module.writeCaseObject("Foo", "foo")
        module.compile.succeeded should be (true)
          
    }
  }
}
