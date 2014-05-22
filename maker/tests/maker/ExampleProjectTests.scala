package maker

import org.scalatest.FunSpec
import maker.project.Module
import maker.utils.FileUtils._
import maker.utils.os.Command
import maker.utils.os.CommandOutputHandler

class ExampleProjectTests extends FunSpec {
  describe("Single module project"){
    it("Should run its tests"){
      //val cmd = Command
      val makerScript = file("bin/maker.sh")
      assert(makerScript.exists, "No maker script")
      val module = new Module(file("examples/single-module-project"), "single")
      assert(module.rootAbsoluteFile.exists, "Module doesn't exist")
      val cmd = Command(
        CommandOutputHandler.NULL,
        Some(module.rootAbsoluteFile),
        makerScript.getAbsolutePath, "-e", "project.test"
      )
      val result = cmd.exec
      assert(result == 0, "Should have executed tests")

    }
  }
}
