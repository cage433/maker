package maker.task.tasks

import org.scalatest.FunSuite
import maker.utils.FileUtils._
import maker.project.TestModule
import maker.project.Project
import maker.build.BuildResult
import maker.project.Module
import maker.task.TaskResult
import java.io.File
import org.scalatest.FreeSpec
import maker.Props
import maker.utils.Implicits.RichString._
import maker.task.compile._
import org.scalatest.GivenWhenThen
import org.scalatest.FunSpec
import org.scalatest._
import org.scalatest.FunSuite
import maker.project.TestModule
import maker.build.BuildManager

class WorkflowCompilationTests extends FunSuite{

  /** for this tests an imperative style seems more natural - reflecting
    * the changing state of a developer's module(s)
    */
  object Nouns {
    var result : BuildManager.TimedResults = null
    var a : Module with TestModule = null
    var b : Module with TestModule = null
    var topLevelProject : Project = null
    var src1 : File = null
    var src2 : File = null
    var test20 : File = null
    var src3 : File = null
    var src1SigHash : Int = -1
    var src1ContentHash : String = null
  }
  import Nouns._

  def writeSrcWithDependencies(p : Module with TestModule, n : Int, upstream : List[Int] = Nil, extraLines : List[String] = Nil) = {
    p.writeSrc(
      "foo/Klass" + n + ".scala",
      """
      package foo
      
      case class Klass%s(%s){
        %s
      }
      """ % (n, upstream.map{i => "x%s : Klass%s" % (i, i)}.mkString(", "), extraLines.mkString("\n"))
    )
  }
  def writeTestWithDependencies(p : Module with TestModule, n : Int, upstream : List[Int] = Nil) = {
    p.writeTest(
      "foo/Test" + n + ".scala",
      """
      package foo
      import org.scalatest.FunSuite
      
      class Test%s extends FunSuite{
        test("can compile"){
          %s
        }
      }
      """ % (n, upstream.map{i => "val x%s : Klass%s = null" % (i, i)}.mkString("\n"))
    )
  }

  test("A developer's workflow"){
    withTempDir{
      dir => 

      info("given a module with two source dirs")
      val props = Props.initialiseTestProps(dir)

      a = TestModule(file(dir, "a").makeDir(), "WorkflowCompilationTests", props)
      info("Compilation should succeed") 
      src1 = writeSrcWithDependencies(a, 1)
      src2 = writeSrcWithDependencies(a, 2, List(1))
      assert(a.testCompile.succeeded)

      info("there should be no test compilation") 
      assert(a.compilePhase.classFiles.nonEmpty)
      assert(a.testCompilePhase.classFiles.isEmpty)

      info("when changing the signature of the upstream source file") 
      src1 = writeSrcWithDependencies(a, 1, Nil, List("def newPublicMethod : Int = 33"))

      info("Compilation should succeed") 
      result = a.compile
      assert(result.succeeded)

      info("after writing a test file")

      info("Compilation should succeed")
      test20 = writeTestWithDependencies(a, 20, List(1, 2))
      result = a.testCompile
      assert(result.succeeded)

      info("With a new module that has an upstream dependency on the first")
      b = TestModule(file(dir, "b").makeDir(), "WorkflowCompilationTests-b", props, List(a))
      src3 = writeSrcWithDependencies(b, 3, List(1))
      topLevelProject = new Project("top", dir, List(b), props)

      info("Test compilation should succeed") 
      result = b.testCompile
      assert(result.succeeded)

      info("when a source file is deleted")
      src3.delete

      info("Compilation should still work")
      result = b.testCompile
      assert(result.succeeded)
    }
  }
}


