package maker.project

import org.scalatest.{Matchers, FreeSpec}
import maker.utils.FileUtils._

class TestModuleTests extends FreeSpec with Matchers{

  "classpath should be entirely contained in root" in {
    withTempDir{
      dir => 
        val module = new TestModule(
          dir,
          "classpath test"
        )
        module.testCompilationClasspathComponents.foreach{
          file => 
            file.isContainedIn(dir) should be (true)
        }


    }
  }
}
