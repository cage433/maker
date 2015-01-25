package maker.task.tasks

import org.scalatest.{FreeSpec, Matchers}
import maker.utils.os.Command
import java.io.File
import maker.project.{Module, TestModule}
import maker.utils.FileUtils._
import maker.utils.{CustomMatchers, Stopwatch}
import maker.MakerProps
import scala.collection.immutable.Nil
import maker.task.compile.SourceCompilePhase

class PackageJarTaskTests extends FreeSpec with Matchers{
  private def createTestModule(dir : File, name : String, upstreamModules : List[Module] = Nil) = {
    val moduleRoot = file(dir, name)
    val props = MakerProps(
      "Compiler", "dummy-test-compiler",
      "RunningInMakerTest", "true"
    )
    new TestModule(
      moduleRoot,
      name,
      overrideProps = Some(props),
      upstreamProjects = upstreamModules
    )
  }

  "Simple module should package classes and resources" in {
    withTestDir{
      dir =>  

        val proj = createTestModule(dir, "single-module-package-jar-test")

        proj.addUnmanagedResource("MainResource1")
        proj.addUnmanagedResource("subdir-b", "MainResource2")

        proj.writeCaseObject("Foo", "foo")

        proj.pack.succeeded should be (true)

        PackageJarTaskTests.checkJarContainsDirectoryContents(
          proj.outputDir(SourceCompilePhase), 
          proj.packageJar(SourceCompilePhase))
        PackageJarTaskTests.checkJarContainsDirectoryContents(
          proj.resourceDir(SourceCompilePhase), 
          proj.packageJar(SourceCompilePhase))
    }
  }

  "Simple module should package source" in {
    withTempDir{
      dir =>  

        val proj = createTestModule(dir, "single-module-package-jar-test")

        proj.addUnmanagedResource("MainResource1")
        proj.addUnmanagedResource("subdir-b", "MainResource2")

        proj.writeCaseObject("Foo", "foo")

        proj.pack

        PackageJarTaskTests.checkJarContainsDirectoryContents(
          proj.sourceDirs(SourceCompilePhase).head,
          proj.sourcePackageJar(SourceCompilePhase))
    }
  }
  "Can package upstream modules into one big jar" in {
    withTempDir{
      dir => 
        val a = createTestModule(dir, "a")
        val b = createTestModule(dir, "b", upstreamModules = List(a))
        val c = createTestModule(dir, "c", upstreamModules = List(b))


        a.writeCaseObject("Foo", "foo")
        a.addUnmanagedResource("a-resource")

        b.writeCaseObject("Bar", "foo")
        b.addUnmanagedResource("b-resource")

        c.writeCaseObject("Baz", "foo")
        c.addUnmanagedResource("c-resource")

        c.packageAllUpstream

        val oneBigJar = c.packageJar(SourceCompilePhase)

        Vector(a, b, c).foreach{
          m => 
            PackageJarTaskTests.checkJarContainsDirectoryContents(
              m.outputDir(SourceCompilePhase),
              oneBigJar
            )
            PackageJarTaskTests.checkJarContainsDirectoryContents(
              m.resourceDir(SourceCompilePhase),
              oneBigJar
            )
        }
    }
  }

}

object PackageJarTaskTests extends Matchers with CustomMatchers{
  def checkJarContainsDirectoryContents(dir : File, jarFile : File){
    val jarContents = {
      val cmd = Command(MakerProps().Jar().getAbsolutePath, "tvf", jarFile.getPath).withSavedOutput
      cmd.exec
      cmd.savedOutput.split("\n")
    }
    val relativePaths = allFiles(dir).filter(_.isFile).map(_.relativeTo(dir).getPath)

    relativePaths should not be empty // Otherwise why are we testing this

    relativePaths should allSatisfy{
      path : String => jarContents.exists(_.contains(path))
    }
  }
}
