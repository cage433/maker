package maker.task.test

import org.scalatest.FunSuite
import maker.utils.FileUtils._
import maker.project.TestModule
import org.scalatest.ParallelTestExecution
import maker.project.Module
import maker.Props

class RunUnitTestsTaskTests extends FunSuite {

  test("Test Reporter does its thing"){
    withTempDir{
      root => 
        val props = Props.initialiseTestProps(root) 

        val proj = TestModule(root, "RunUnitTestsTaskTests", props)
        appendToFile(
          file(root, "external-resources"),
          """|org.scalatest scalatest_{scala_version_base} {scalatest_version}""".stripMargin
        )
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

        proj.writeTest(
          "foo/ExceptionThrowingTest.scala",
          """
          package foo
          import org.scalatest.FunSuite
          class ExceptionThrowingTest extends FunSuite{
            test("test throwing exception"){
              throw(new RuntimeException("error"))
            }
          }
          """
        )


        var testResults = proj.test.testResults
        assert(testResults.numFailedTests === 2, "Expecting exactly two failures")
        assert(testResults.numPassedTests === 2, "Expecting exactly two passes")

        //Running failed tests should pass over passed tests
        testResults = proj.testFailedSuites.testResults

        assert(testResults.numFailedTests === 2, "Expecting exactly two failures")
        assert(testResults.numPassedTests === 0, "Expecting zero passes")

        //repair broken tests
        proj.writeTest(
          "foo/ExceptionThrowingTest.scala",
          """
          package foo
          import org.scalatest.FunSuite
          class ExceptionThrowingTest extends FunSuite{
            test("test throwing exception"){
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
              assert(2 === 2)
            }
          }
          """
        )
        testResults = proj.testFailedSuites.testResults
        assert(testResults.numFailedTests === 0, "Expecting zero failures")
        assert(testResults.numPassedTests === 2, "Expecting two passes")
    }
  }

}
