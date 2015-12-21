package maker.task.tasks

import maker.utils.FileUtils._
import maker.utils.os.Command
import org.scalatest._
import maker.project.{TestModule, TestModuleBuilder}
import org.slf4j.LoggerFactory
import maker.TestMakerRepl

class RunUnitTestsTaskTests extends FreeSpec with Matchers with ParallelTestExecution
  with BeforeAndAfterAll
{

  override def beforeAll(){
    val logger = LoggerFactory.getLogger(getClass)
  }

  "Unit test runs" in {
    withTempDir{
      rootDirectory => 

        writeToFile(
          file(rootDirectory, "src/foo/Foo.scala"), 
          """
          package foo
          case class Foo(x : Double){
            val fred = 10
            def double() = x + x
          }
          """
        )
        writeToFile(
          file(rootDirectory, "tests/foo/FooTest.scala"),
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
        writeToFile(
          file(rootDirectory, "Project.scala"),
          s"""
            import maker.project._
            import maker.utils.FileUtils._
            import maker.ScalaVersion

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
        repl.inputLine("val res = a.test")
        repl.waitForRepl()
        repl.inputLine("System.exit(if(res.succeeded) 0 else 1)")
        repl.exitValue() should be (0)
    }
  }

  "Broken tests fail" in {
    withTempDir{
      rootDirectory => 
        writeToFile(
          file(rootDirectory, "tests/foo/Test.scala"),
          """
            package foo

            import org.scalatest.FunSuite

            class Test extends FunSuite{
              test("Deliberately broken test"){
                assert(1 === 2)
              }
            }
          """
        )
        writeToFile(
          file(rootDirectory, "Project.scala"),
          s"""
            import maker.project._
            import maker.utils.FileUtils._
            import maker.ScalaVersion

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
        repl.inputLine("val res = a.test")
        repl.waitForRepl()
        repl.makerOutput().contains("1 did not equal 2") should be (true)
        repl.inputLine("System.exit(if(res.succeeded) 1 else 0)")
        repl.exitValue() should be (0)
    }
  }


  "Can re-run failing tests" ignore {
    withTempDir{
      root => 
        TestModuleBuilder.createMakerProjectFile(root)

        // these files will be created by the module funcitons - as a sanity check that they run
        val file1 = file(root, "file1")
        val file2 = file(root, "file2")
        file1.exists should be (false)
        file2.exists should be (false)

        val module = new TestModuleBuilder(root, "RunUnitTestsTaskTestsModule3").
        withExtraCode(
          s"""|
              | import maker.utils.FileUtils._
              | def checkTestsWhenOneIsBroken{
              |   test
              |   assert(testResults.failedTests.size == 1, "Should have exactly one failure in first run")
              |   assert(testResults.passedTests.size == 1, "Should have exactly one pass in first run")
              |
              |   testFailedSuites
              |   assert(testResults.failedTests.size == 1, "Should have exactly one failure when first running failed suites")
              |   assert(testResults.passedTests.size == 0, "Should have exactly one pass when first running failed suites")
              |   file("${file1.absPath}").touch
              | }
              |
              | def checkTestsAfterFix{
              |   testFailedSuites
              |   assert(testResults.failedTests.size == 0, "Should have no failures")
              |   assert(testResults.passedTests.size == 1, "Should have exactly one pass")
              |   test
              |   assert(testResults.failedTests.size == 0, "Should have no failures")
              |   assert(testResults.passedTests.size == 2, "Should have exactly two pass")
              |   file("${file2.absPath}").touch
              | }
              |""".stripMargin
        ).withNoExecModeExit

        module.appendDefinitionToProjectFile(root)

        module.writeTest(
          "foo/GoodTest.scala",
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
        module.writeTest( 
          "foo/BadTest.scala",
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

        var command = TestModuleBuilder.makerExecuteCommand(
          root, 
          "RunUnitTestsTaskTestsModule3.checkTestsWhenOneIsBroken"
        ).withNoOutput

        var result = command.run
        result should equal (0)
        file1.exists should be (true)

        //Repair the broken test, check there is one passing test
        module.writeTest( 
          "foo/BadTest.scala",
          """
          package foo
          import org.scalatest.FunSuite
          class BadTest extends FunSuite{
            test("this test has been fixed"){
              assert(1 === 1)
            }
          }
          """
        )
        command = TestModuleBuilder.makerExecuteCommand(
          root, 
          "RunUnitTestsTaskTestsModule3.checkTestsAfterFix"
        ).withNoOutput

        result = command.run
        result should equal (0)
        file2.exists should be (true)
    }
  }

}
