package maker.task.tasks

import org.scalatest.{Matchers, FreeSpec}
import maker.TestMakerRepl
import maker.utils.FileUtils
import java.io.File

class PublishLocalTaskTests extends FreeSpec with Matchers with FileUtils {

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
        val projectOrg = "maker_test_org"
        val projectName = "maker_test_project"
        val projectVersion = "1.3"
        TestMakerRepl.writeProjectFile(
          rootDirectory,
          s"""
            lazy val a = new Module(
              root = file("$rootDirectory"),
              name = "a"
            ) with ClassicLayout 

            lazy val p = new Project(
              "$projectOrg",
              "$projectName",
              file("$rootDirectory"),
              Seq(a)
            ) 
          """
        )
        val repl = TestMakerRepl(rootDirectory, true)
        repl.inputLine(s"""val result = p.publishLocal2(version = "$projectVersion", signArtifacts = true)""")

        val resourceCacheDirectory: File = {
          mkdirs(file(System.getProperty("user.home"), ".maker", "resource-cache"))
        }
        val publishLocalDir = file(resourceCacheDirectory, projectOrg, projectName, projectVersion)
        publishLocalDir.exists should be (true)
        Seq(
          s"$projectName-$projectVersion.jar",
          s"$projectName-$projectVersion-sources.jar",
          s"$projectName-$projectVersion-javadoc.jar",
          s"$projectName-$projectVersion.pom"
        ).foreach {
          basename => 
            file(publishLocalDir, basename).exists should be (true)
            file(publishLocalDir, s"$basename.asc").exists should be (true)
        }

        recursiveDelete(file(resourceCacheDirectory, projectOrg))
        repl
    }
  }
}
