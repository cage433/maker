package maker.task.test

import org.scalatest.FunSuite
import maker.utils.FileUtils._
import maker.project.TestModule
import org.scalatest.ParallelTestExecution
import maker.project.Module
import maker.Props

class RunUnitTestsTaskTests extends FunSuite {

  test("Test Reporter does its thing"){
    withTestDir{
      root => 
        val props = Props.initialiseTestProps(root) 

        val proj = TestModule(root, "RunUnitTestsTaskTests", props)
        appendToFile(
          file(root, "external-resources"),
          """|org.scalatest scalatest_{scala_version_base} {scalatest_version}
             |org.testng testng 6.2.1
             |com.beust jcommander 1.12
             |org.beanshell bsh 2.0b4
             |com.google.inject guice 2.0""".stripMargin
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
          "foo/TestNGTest.scala",
          """
          package foo
          import org.scalatest.testng.TestNGSuite
          import org.testng.annotations.Test
          class TestNGTest extends TestNGSuite {
            @Test
            def testAnything{
              assert(1 == 1)
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
        assert(testResults.numPassedTests === 3, "Expecting exactly three passes")

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
