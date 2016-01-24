package maker.task.tasks

import org.scalatest.{Matchers, FreeSpec}
import maker.TestMakerRepl
import maker.utils.FileUtils

class PublishLocalTask2Tests extends FreeSpec with Matchers with FileUtils {

  "Can publish locally" in {
    withTestDir {
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
              "com.org",
              "p",
              file("$rootDirectory"),
              Seq(a)
            ) 
          """
        )
        val repl = TestMakerRepl(rootDirectory, true)
        repl.inputLine(s"""System.getenv("MAKER_GPG_PASS_PHRASE")""")
        repl.inputLine(s"""p.publishLocal2(version = "1.3", signArtifacts = true)""")
    }
  }
}
