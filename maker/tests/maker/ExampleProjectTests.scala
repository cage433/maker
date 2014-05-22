package maker

import org.scalatest.FunSpec
import maker.project.Module
import maker.utils.FileUtils._

class ExampleProjectTests extends FunSpec {
  describe("Single module project"){
    it("Should run its tests"){
      //val cmd = Command
      val module = new Module(file("examples/single-project"), "single")
    }
  }
}
