package maker

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import org.scalatest.FunSpec
import maker.project.Module
import maker.utils.FileUtils._
import maker.utils.os.Command
import java.io.ByteArrayOutputStream


class ExampleProjectTests extends FunSpec {
  val makerScript = file("maker.py").getAbsolutePath

  describe("Single module project"){
    it("Should run its tests"){
      val moduleRootFile = file("examples/single-module-project")
      assert(moduleRootFile.exists, "Module doesn't exist")
      val bs = new ByteArrayOutputStream()
      val cmd = Command("./run-tests.sh").withWorkingDirectory(moduleRootFile).withOutputTo(bs).withExitValues(0, 255)
      val result = cmd.run
      assert(result == 0, "Should have executed tests - output was " + bs.toString)
    }
  }
  
  describe("Multi module project"){
    it("Should run its tests"){
      val moduleRootFile = file("examples/multi-module-project")
      assert(moduleRootFile.exists, "Module doesn't exist")
      val bs = new ByteArrayOutputStream()
      val cmd = Command("./run-tests.sh").withWorkingDirectory(moduleRootFile).withOutputTo(bs).withExitValues(0, 255)
      val result = cmd.run
      assert(result == 0, "Should have executed tests - output was " + bs.toString)
    }
  }
}
