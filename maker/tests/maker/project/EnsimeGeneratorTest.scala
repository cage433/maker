package maker.project

import maker.utils.FileUtils._
import org.apache.commons.io.{ FileUtils => ApacheFileUtils }
import org.scalatest.FreeSpec

class EnsimeGeneratorTests extends FreeSpec {
  "Generate .ensime for a simple project" in {
    withTempDir { dir =>
      val module = new TestModule(dir, "testEnsimeGenerator")
      val project = new Project("parent", dir, List(module))

      project.generateEnsimeProject()

      val config = file(dir, ".ensime")

      assert(config.exists(), ".ensime should have been created")

      val lisp = ApacheFileUtils.readFileToString(config)

      // no schema to test against
      assert(lisp.count(_ == '(') === lisp.count(_ == ')'), "imbalanced braces in .ensime")
    }
  }
}
