package maker.task.compile

import org.scalatest.{Matchers, FreeSpec}
import maker.utils.FileUtils
import maker.project.TestModule
import maker.Resource

class CrossCompilationTests extends FreeSpec with Matchers{
  import FileUtils.{withTempDir, writeToFile, file, withTestDir}
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
            Resource("org.scala-lang",name, "2.9.3", Some(module.projectScalaLibsDir)).update()
        }
        val res = module.compile
        res.succeeded should be (true)
          
    }
  }
}
