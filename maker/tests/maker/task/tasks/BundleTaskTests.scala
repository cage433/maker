package maker.task.tasks

import org.scalatest.{Matchers, FreeSpec}
import maker.utils.FileUtils._
import maker.project.TestModule
import maker.utils.os.Command

class BundleTaskTests extends FreeSpec with Matchers{
  "A single module should bundle just itself" in {
    withTestDir{
      dir =>
        val module = new TestModule(dir, "BundleTaskTests.singleModule")
        module.writeCaseObject("foo", "Foo")
        module.bundle(version = "1.0", signArtifacts = false) should be ('succeeded)
        module.bundleJar should be ('exists)

        val jarCommand = Command(module.props.Jar().getAbsolutePath, "tvf", module.bundleJar.getAbsolutePath).withSavedOutput
        jarCommand.exec should be (0)

        val contents = jarCommand.savedOutput
        val expectedContents = 
          "pom.xml" :: 
          List(module.publishLocalJar, module.publishLocalSourceJar,  module.docPackageJar).map(_.basename)

        expectedContents.foreach{
          expected => 
            assert(contents.contains(expected), "Couldn't find $expected")
        }
    }
  }
}
