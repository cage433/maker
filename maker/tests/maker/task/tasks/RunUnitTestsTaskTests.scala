package maker.task.tasks

import org.scalatest.FunSuite
import maker.utils.FileUtils._
import maker.project.TestModule
import org.scalatest.ParallelTestExecution
import maker.project.Module
import maker.MakerProps

class RunUnitTestsTaskTests extends FunSuite {
  ignore("Test reports picks up failure"){
    withTempDir{
      dir ⇒ 
        val proj = new TestModule(dir, "RunUnitTestsTaskTests")
        proj.writeTest(
          "foo/Test.scala",
          """
            package foo

            import org.scalatest.FunSuite

            class Test extends FunSuite{
              test("This should fail"){
                assert(1 === 2)
              }
            }
          """
        )
        proj.test
        assert(proj.testOutputFile.exists, "Test output should exist")
    }
  }


  ignore("Unit test runs"){
    withTempDir{
      root ⇒ 
        val proj = new TestModule(root, "RunUnitTestsTaskTests")

        proj.writeSrc(
          "foo/Foo.scala", 
          """
          package foo
          case class Foo(x : Double){
            val fred = 10
            def double() = x + x
          }
          """
        )
        proj.writeTest(
          "foo/FooTest.scala",
          """
          package foo
          import org.scalatest.FunSuite
          class FooTest extends FunSuite{
            test("test foo"){
              val foo1 = Foo(1.0)
              val foo2 = Foo(1.0)
              assert(foo1 === foo2)
            }
          }
          """
        )
        assert(proj.test.succeeded)
    }
  }

  ignore("Failing test fails again"){
    withTempDir{
      root ⇒ 
        val proj = new TestModule(root, "RunUnitTestsTaskTests")
        proj.writeSrc(
          "foo/Foo.scala", 
          """
          package foo
          case class Foo(x : Double){
            val fred = 10
            def double() = x + x
          }
          """
        )
        val testFile = proj.writeTest(
          "foo/FooTest.scala",
          """
          package foo
          import org.scalatest.FunSuite
          import java.io._
          class FooTest extends FunSuite{
            val f = new File(".")
            test("test foo"){
              assert(1 === 2)
            }
          }
          """
        )
        assert(proj.testCompile.succeeded, "Expected compilation to succeed")

        assert(proj.test.failed, "Expected test to fail")
    }
  }

  ignore("Can re-run failing tests"){
    withTempDir{
      root ⇒ 
        val proj = new TestModule(root, "RunUnitTestsTaskTests")

        proj.writeTest(
          "foo/GoodTest.scala",
          """
          package foo
          import org.scalatest.FunSuite
          class GoodTest extends FunSuite{
            test("test foo"){
              assert(1 === 1)
            }
          }
          """
        )
        proj.writeTest( 
          "foo/BadTest.scala",
          """
          package foo
          import org.scalatest.FunSuite
          class BadTest extends FunSuite{
            test("test foo"){
              assert(1 === 2)
            }
          }
          """
        )

        proj.test
        assert(proj.testResults.failedTests.size === 1, "Expecting exactly one failure")
        assert(proj.testResults.passedTests.size === 1, "Expecting exactly one pass")

        //This time we should only run the failed test
        //so there should be no passing tests
        proj.testFailedSuites
        assert(proj.testResults.failedTests.size === 1)
        assert(proj.testResults.passedTests.size === 0)

        //Repair the broken test, check there is one passing test
        proj.writeTest( 
          "foo/BadTest.scala",
          """
          package foo
          import org.scalatest.FunSuite
          class BadTest extends FunSuite{
            test("test foo"){
              assert(1 === 1)
            }
          }
          """
        )
        proj.testFailedSuites
        assert(proj.testResults.failedTests.size === 0)
        assert(proj.testResults.passedTests.size === 1)

        //Re-run failed tests - should do nothing
        proj.testFailedSuites
        assert(proj.testResults.failedTests.size === 0)
        assert(proj.testResults.passedTests.size === 1)


        //Run all tests - should have two passes
        proj.test
        assert(proj.testResults.failedTests.size === 0)
        assert(proj.testResults.passedTests.size === 2)

    }
  }

  test("Test Reporter does its thing"){
    withTempDir{
      root ⇒ 
        val overrideProps = Some(TestModule.makeTestProps(root) ++ 
          ("TestReporter","maker.scalatest.MakerTestReporter2", 
          "MakerTestReporterClasspath", "test-reporter/target-maker/classes/"))
        val proj = new TestModule(root, "RunUnitTestsTaskTests", Nil, Nil, overrideProps)
        proj.writeTest(
          "foo/GoodTest.scala",
          """
          package foo
          import org.scalatest.FunSuite
          class GoodTest extends FunSuite{
            test("test foo"){
              assert(1 === 1)
            }
            test("test bar"){
              assert(2 == 2)
            }
          }
          """
        )
        proj.writeTest( 
          "foo/BadTest.scala",
          """
          package foo
          import org.scalatest.FunSuite
          class BadTest extends FunSuite{
            test("test foo"){
              assert(1 === 2)
            }
          }
          """
        )
        proj.test
        val testResultDir = file(proj.testResultDirectory)
        assert(testResultDir.exists)
        assert(file(testResultDir, "starttime").exists, "Test run start time file should exist")
        val goodSuiteDir = file(testResultDir, "foo.GoodTest")
        val badSuiteDir = file(testResultDir, "foo.BadTest")
        val suiteDirs = List(goodSuiteDir, badSuiteDir)
        suiteDirs.foreach{
          dir => 
            assert(dir.exists, dir + " should exist")
            assert(file(dir, "starttime").exists, "start time should exist")
            assert(file(dir, "state").readLines.head === "complete")
        }
        
        val goodTestDirs = List(
          file(goodSuiteDir, "test foo"),
          file(goodSuiteDir, "test bar")
        )
        val badTestDirs = List(
          file(badSuiteDir, "test foo")
        )
        (goodTestDirs ::: badTestDirs).foreach{
          dir =>
            assert(dir.exists, dir + " should exist")
            assert(file(dir, "starttime").exists, "start time should exist")
        }
        goodTestDirs.foreach{
          dir =>
            val stateFile = file(dir, "state")
            assert(stateFile.exists)
            assert(stateFile.readLines.head === "succeeded")
        }
        badTestDirs.foreach{
          dir =>
            val stateFile = file(dir, "state")
            assert(stateFile.exists)
            assert(stateFile.readLines.head === "failed")
        }
    }
  }

}
