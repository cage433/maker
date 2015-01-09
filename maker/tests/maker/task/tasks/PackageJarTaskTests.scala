package maker.task.tasks

import org.scalatest.{FreeSpec, Matchers}
import maker.utils.os.Command
import java.io.File
import maker.project.{Module, TestModule}
import maker.utils.FileUtils._
import maker.utils.CustomMatchers
import maker.MakerProps
import scala.collection.immutable.Nil

class PackageJarTaskTests extends FreeSpec{
  private def createTestModule(dir : File, name : String, upstreamModules : List[Module] = Nil) = {
    val moduleRoot = file(dir, name)
    val props = MakerProps(
      "Compiler", "dummy-test-compiler"
    )
    new TestModule(
      moduleRoot,
      name,
      overrideProps = Some(props),
      upstreamProjects = upstreamModules
    )
  }

  "Simple module should package classes and resources" in {
    withTempDir{
      dir =>  

        val proj = createTestModule(dir, "single-module-package-jar-test")

        proj.addUnmanagedResource("MainResource1")
        proj.addUnmanagedResource("subdir-b", "MainResource2")

        proj.writeCaseObject("Foo", "foo")

        proj.pack

        PackageJarTaskTests.checkJarContainsDirectoryContents(proj, proj.outputDir, proj.outputArtifact)
        PackageJarTaskTests.checkJarContainsDirectoryContents(proj, proj.resourceDir, proj.outputArtifact)
    }
  }
}

object PackageJarTaskTests extends Matchers with CustomMatchers{
  def checkJarContainsDirectoryContents(module : Module, dir : File, jarFile : File){
    val jarContents = {
      val cmd = Command(module.props.Jar().getAbsolutePath, "tvf", jarFile.getPath).withSavedOutput
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
