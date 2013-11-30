package maker.task.test

import org.scalatest.FunSuite
import maker.utils.FileUtils._
import maker.Props
import maker.project.Module
import maker.project.TestModule

class AkkaTestManagerTests extends FunSuite {
  test("Can receive test events via akka"){
    withTempDir{
      dir => 
        val props = Props.initialiseTestProps(dir)
        val moduleName = "AkkaTestReporterTest"
      //val testManager = new AkkaTestManager()
        val module = new Module(
          dir, moduleName, 
          props
        ) with TestModule{
        }

        module.writeTest(
          "foo/Test1.scala",
          """
            package foo

            import org.scalatest.FunSuite

            class Test1 extends FunSuite{
              test("a passing test"){
                assert(1 === 1)
              }
              test("a failing test"){
                assert(1 === 2)
              }
            }
          """
        )

      assert(module.testCompile.succeeded, "Should compile")
      val res = module.test
      var testResults = res.testResults
      assert(res.failed, "Tests should have failed")

      assert(testResults.numPassedTests === 1, "One successful test")
      assert(testResults.numFailedTests === 1, "One failing test")
      assert(testResults.failedTestSuites === List("foo.Test1"), "One suite failed")
      
      testResults = module.test.testResults
      assert(testResults.numPassedTests === 1, "One successful test")
      assert(testResults.numFailedTests === 1, "One failing test")
    }
  }
    
}

