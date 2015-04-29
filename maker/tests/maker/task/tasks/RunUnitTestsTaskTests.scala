package maker.task.tasks

import maker.utils.FileUtils._
import maker.utils.os.Command
import org.scalatest.{Matchers, FunSuite, ParallelTestExecution}
import maker.project.{TestModule, TestModuleBuilder}

class RunUnitTestsTaskTests extends FunSuite with Matchers with ParallelTestExecution{

  test("Broken tests fail"){
    withTestDir{
      dir => 
        TestModuleBuilder.createMakerProjectFile(dir)
        val module = new TestModuleBuilder(dir, "RunUnitTestsTaskTestsModule")

        module.appendDefinitionToProjectFile(dir)
        module.writeTest(
          "foo/Test.scala",
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
        val testOutputFile = file(dir, "maker-test-output")
        testOutputFile.exists should be (false)
        val command = TestModuleBuilder.makerExecuteCommand(
          dir, 
          "RunUnitTestsTaskTestsModule.test"
        )//.withNoOutput

        val result = command.run
        result should equal (1)
        testOutputFile.exists should be (true)
    }
  }


  ignore("Unit test runs"){
    withTempDir{
      root => 
        TestModuleBuilder.createMakerProjectFile(root)
        val module = new TestModuleBuilder(root, "RunUnitTestsTaskTestsModule2")

        module.appendDefinitionToProjectFile(root)

        module.writeSrc(
          "foo/Foo.scala", 
          """
          package foo
          case class Foo(x : Double){
            val fred = 10
            def double() = x + x
          }
          """
        )
        module.writeTest(
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
        val command = TestModuleBuilder.makerExecuteCommand(
          root, 
          "RunUnitTestsTaskTestsModule2.test"
        ).withNoOutput

        val result = command.run
        result should equal (0)
    }
  }


  ignore("Can re-run failing tests"){
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
