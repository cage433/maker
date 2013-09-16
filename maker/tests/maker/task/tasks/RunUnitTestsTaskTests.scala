package maker.task.tasks

import org.scalatest.FunSuite
import maker.utils.FileUtils._
import maker.project.TestModule
import org.scalatest.ParallelTestExecution
import maker.project.Module
import maker.MakerProps

class RunUnitTestsTaskTests extends FunSuite {

  test("Test Reporter does its thing"){
    withTempDir{
      root => 
        val props = MakerProps.initialiseTestProps(root,
          "TestReporter","maker.scalatest.MakerTestReporter", 
          "MakerTestReporterClasspath", "test-reporter/target-maker/classes/")

        val proj = new TestModule(root, "RunUnitTestsTaskTests", props)
        file("resource-resolvers").copyTo(root)
        file("resource-versions").copyTo(root)
        writeToFile(
          file(root, "external-resources"),
          """|org.scalatest scalatest_{scala_version} {scalatest_version}
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


        proj.test
        assert(proj.testResults.failedTests.size === 2, "Expecting exactly two failures")
        assert(proj.testResults.passedTests.size === 3, "Expecting exactly three passes")

        val testResultDir = file(proj.testResultDirectory)
        assert(testResultDir.exists, testResultDir.absPath + " should exist")
        assert(file(testResultDir, "starttime").exists, "Test run start time file should exist")
        val goodSuiteDir = file(testResultDir, "suites/foo.GoodTest")
        val goodTestNGSuiteDir = file(testResultDir, "suites/foo.TestNGTest")
        val exceptionSuiteDir = file(testResultDir, "suites/foo.ExceptionThrowingTest")
        val badSuiteDir = file(testResultDir, "suites/foo.BadTest")
        val suiteDirs = List(goodSuiteDir, goodTestNGSuiteDir, badSuiteDir, exceptionSuiteDir)

        suiteDirs.foreach{
          dir => 
            assert(dir.exists, dir + " should exist")
            assert(file(dir, "starttime").exists, "start time should exist")
            assert(file(dir, "endtime").exists, "end time should exist")
        }
        
        val goodTestDirs = List(
          file(goodSuiteDir, "tests/test foo"),
          file(goodSuiteDir, "tests/test bar"),
          file(goodTestNGSuiteDir, "tests/testAnything")
        )
        val badTestDirs = List(
          file(badSuiteDir, "tests/test foo"),
          file(exceptionSuiteDir, "tests/test throwing exception")
        )
        (goodTestDirs ::: badTestDirs).foreach{
          dir =>
            assert(dir.exists, dir + " should exist")
            assert(file(dir, "starttime").exists, "start time should exist")
        }

        goodTestDirs.foreach{
          dir =>
            val stateFile = file(dir, "status")
            assert(stateFile.exists)
            assert(stateFile.readLines.head === "succeeded")
            val exceptionFile = file(dir, "exception")
            assert(! exceptionFile.exists)
            val messageFile = file(dir, "message")
            assert(! messageFile.exists)
        }

        badTestDirs.foreach{
          dir =>
            val stateFile = file(dir, "status")
            assert(stateFile.exists)
            assert(stateFile.readLines.head === "failed")

            val exceptionFile = file(dir, "exception")
            assert(exceptionFile.exists)
            val messageFile = file(dir, "message")
            assert(messageFile.exists)
        }

        // Running failed tests should pass over passed tests
        proj.testFailedSuites
        assert(proj.testResults.failedTests.size === 2, "Expecting exactly two failures")
        assert(proj.testResults.passedTests.size === 0, "Expecting zero passes")

        // repair broken tests
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
        proj.testFailedSuites
        assert(proj.testResults.failedTests.size === 0, "Expecting zero failures")
        assert(proj.testResults.passedTests.size === 2, "Expecting two passes")
    }
  }

}
