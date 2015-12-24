package maker.task.tasks

import maker.utils.FileUtils._
import maker.utils.os.Command
import org.scalatest._
import maker.{TestMakerRepl, Log}
import maker.utils.FileUtils
import maker.project.Module
import maker.task.compile.{TestCompilePhase, CompileTask, SourceCompilePhase}

class RunUnitTestsTaskTests extends FreeSpec with Matchers with ParallelTestExecution
  with BeforeAndAfterAll
  with FileUtils 
  with Log
{



  "Unit test workflow is as expected" ignore {
    withTempDir{
      rootDirectory => 

        writeToFile(
          file(rootDirectory, "tests/foo/GoodTest.scala"),
          """
          package foo
          import org.scalatest.FunSuite
          class GoodTest extends FunSuite{
            test("this is a good test"){
              assert(1 === 1)
            }
          }
          """
        )

        writeToFile( 
          file(rootDirectory, "tests/foo/BadTest.scala"),
          """
          package foo
          import org.scalatest.FunSuite
          class BadTest extends FunSuite{
            test("this test should fail"){
              assert(1 === 2)
            }
          }
          """
        )
        TestMakerRepl.writeProjectFile(
          rootDirectory,
          s"""
            lazy val a = new Module(
              root = file("$rootDirectory"),
              name = "a"
            ) with ClassicLayout {
              override def dependencies = Seq(
                "org.scalatest" % "scalatest" %%  "2.2.0"
              )
            }
          """
        )

        val repl = TestMakerRepl(rootDirectory)
        repl.inputLine("a.test")
        repl.value("a.testResults.failedTests.size").toInt should be (1)
        repl.value("a.testResults.passedTests.size").toInt should be (1)

        repl.inputLine("a.testFailedSuites")
        repl.value("a.testResults.failedTests.size").toInt should be (1)
        repl.value("a.testResults.passedTests.size").toInt should be (0)


        writeToFile( 
          file(rootDirectory, "tests/foo/BadTest.scala"),
          """
          package foo
          import org.scalatest.FunSuite
          class BadTest extends FunSuite{
            test("this test should fail"){
              assert(1 === 1)
            }
          }
          """
        )

        repl.inputLine("a.testFailedSuites")
        repl.value("a.testResults.failedTests.size").toInt should be (0)
        repl.value("a.testResults.passedTests.size").toInt should be (1)

        repl.inputLine("a.test")
        repl.value("a.testResults.failedTests.size").toInt should be (0)
        repl.value("a.testResults.passedTests.size").toInt should be (2)
    }
  }


}
