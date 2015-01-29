package maker

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

import org.scalatest.FunSpec
import maker.project.Module
import maker.utils.FileUtils._
import maker.utils.os.Command
import maker.utils.os.CommandOutputHandler

class ExampleProjectTests extends FunSpec {
  val makerScript = file("bin/maker.sh").getAbsolutePath

  describe("Single module project"){
    it("Should run its tests"){
      val moduleRootFile = file("examples/single-module-project")
      assert(moduleRootFile.exists, "Module doesn't exist")
      val cmd = Command(
        CommandOutputHandler.NULL,
        Some(moduleRootFile),
        true,
        makerScript, "-e", "project.test"
      )
      val result = cmd.exec
      assert(result == 0, "Should have executed tests")
    }
  }
  
  describe("Multi module project"){
    it("Should run its tests"){
      val moduleRootFile = file("examples/multi-module-project")
      assert(moduleRootFile.exists, "Module doesn't exist")
      val script = file(moduleRootFile, "run-tests.sh").getAbsolutePath
      val cmd = Command(
        CommandOutputHandler.NULL,
        Some(moduleRootFile),
        true, 
        script
      )
      val result = cmd.exec
      assert(result == 0, "Should have executed tests")
    }
  }
}
