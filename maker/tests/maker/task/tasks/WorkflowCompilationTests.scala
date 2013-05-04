package maker.task.tasks

import org.scalatest.FunSuite
import maker.utils.FileUtils._
import maker.project.TestProject
import maker.project.TopLevelProject
import maker.task.BuildResult
import maker.project.Project
import maker.task.TaskResult
import java.io.File
import org.scalatest.FreeSpec
import maker.MakerProps
import maker.utils.RichString._
import maker.task.compile._

class WorkflowCompilationTests extends FreeSpec{

  /** for this tests an imperative style seems more natural - reflecting
    * the changing state of a developer's project(s)
    */
  object Nouns {
    var result : BuildResult = null
    var a : TestProject = null
    var b : TestProject = null
    var topLevelProject : TopLevelProject = null
    var src1 : File = null
    var src2 : File = null
    var test20 : File = null
    var src3 : File = null
    var src1SigHash : Int = -1
    var src1ContentHash : String = null
  }
  import Nouns._

  def writeSrcWithDependencies(p : TestProject, n : Int, upstream : List[Int] = Nil, extraLines : List[String] = Nil) = {
    p.writeSrc(
      "foo/Klass" + n + ".scala",
      """
      package foo
      
      case class Klass%s(%s){
        %s
      }
      """ % (n, upstream.map{i ⇒ "x%s : Klass%s" % (i, i)}.mkString(", "), extraLines.mkString("\n"))
    )
  }
  def writeTestWithDependencies(p : TestProject, n : Int, upstream : List[Int] = Nil) = {
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
      """ % (n, upstream.map{i ⇒ "val x%s : Klass%s = null" % (i, i)}.mkString("\n"))
    )
  }


  withTempDir{
    dir ⇒ 
      "A developer's workflow" - {

        "With a project with two source files" - {
          "Compilation should succeed " in {
            a = new TestProject(mkdirs(file(dir, "a")), "WorkflowCompilationTests", props = TestProject.makeTestProps(MakerProps()))
            a.writeMakerFile
            src1 = writeSrcWithDependencies(a, 1)
            src2 = writeSrcWithDependencies(a, 2, List(1))
            assert(a.testCompile.succeeded)
          }
          "Only source compilation should have occured, no test phase" in {
            assert(a.compilePhase.classFiles.nonEmpty)
            assert(a.testCompilePhase.classFiles.isEmpty)
          }
        }

        "when changing the signature of the upstream source file" - {
          "Compilation should succeed " in {
            src1 = writeSrcWithDependencies(a, 1, Nil, List("def newPublicMethod : Int = 33"))

            result = a.compile
            assert(result.succeeded)
          }
        }


       "With a test file" - {
         "Compilation should succeed" in {
           test20 = writeTestWithDependencies(a, 20, List(1, 2))
           result = a.testCompile
           assert(result.succeeded)
         }
       }

        "With a new project that has an upstream dependency on the first" - {
          "Compilation should succeed" in {
            b = new TestProject(mkdirs(file(dir, "b")), "WorkflowCompilationTests-b", List(a))
            src3 = writeSrcWithDependencies(b, 3, List(1))
            topLevelProject = new TopLevelProject("top", List(b), MakerProps("MakerLogLevel", "ERROR", "StripInfoFromTaskResults", "false")) 
            topLevelProject.writeMakerFile
            result = b.testCompile
            assert(result.succeeded)
          }
        }

        "After deleting a source file" - {
          "Compilation should still work" in {
            src3.delete
            result = b.testCompile
            assert(result.succeeded)
          }
        }

    }

  }
}


