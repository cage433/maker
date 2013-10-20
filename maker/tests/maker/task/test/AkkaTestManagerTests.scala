package maker.task.test

import org.scalatest.FunSuite
import maker.utils.FileUtils._
import maker.Props
import maker.project.Module
import maker.project.TestModule
import maker.project.MakerTestReporter

class AkkaTestManagerTests extends FunSuite {
  test("Dummy"){
  }
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
        //override def makerTestReporter = new MakerTestReporter{
          //def scalatestReporterClass = "maker.scalatest.AkkaTestReporter"
          //def scalatestClasspah = file("test-reporter/target-maker/classes").absPath
          //def systemProperties : List[String]  = List(
            //"-Dmaker.test.manager.port=" + testManager.port,
            //"-Dmaker.test.module=" + moduleName
            //)
          //def results() = testManager
          //}
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
      assert(module.test.failed, "Tests should have failed")

      assert(module.testResults.numPassedTests === 1, "One successful test")
      assert(module.testResults.numFailedTests === 1, "One failing test")
      assert(module.testResults.failedTestSuites === List("foo.Test1"), "One suite failed")
      
      module.test
      assert(module.testResults.numPassedTests === 1, "One successful test")
      assert(module.testResults.numFailedTests === 1, "One failing test")
    }
  }
    
}

