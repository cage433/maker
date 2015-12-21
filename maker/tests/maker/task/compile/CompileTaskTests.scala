package maker.task.compile

import org.scalatest._
import java.io.{File, ByteArrayOutputStream, BufferedInputStream}
import maker.utils.FileUtils._
import maker.utils.FileUtils
import maker.project.Module._
import scalaz.syntax.id._
import ch.qos.logback.classic.Level._
import scala.collection.mutable.ListBuffer
import maker.utils.RichString._
import maker.project._
import java.util.concurrent.ConcurrentHashMap
import sbt.inc.Analysis
import com.typesafe.zinc.Compiler
import maker.{TestMaker, TestMakerRepl, ScalaVersion}
import maker.ScalaVersion._
import scala.language.reflectiveCalls
import xsbti.api.This

class CompileTaskTests 
  extends FreeSpec 
  with ParallelTestExecution with TestUtils with FileUtils with Matchers with ModuleTestPimps
{

  "Can compile a single module project" in {
    withTempDir{
      rootDirectory => 
        writeToFile(
          file(rootDirectory, "src/foo/Foo.scala"),
          """
          package foo

          case class Foo(a: Int)
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
              name = "a",
              scalaVersion = ScalaVersion.TWO_ELEVEN_DEFAULT
            ) with ClassicLayout 

            lazy val p = new Project(
              name = "project",
              root = file("$rootDirectory"),
              immediateUpstreamModules = Seq(a)
            )
          
          """
        )

        val repl = TestMakerRepl(rootDirectory)
        findClasses(rootDirectory) should be (empty)
        repl.inputLine("val res = p.compile.succeeded")
        repl.inputLine("System.exit(if (res) 0 else 1)")
        println(repl.makerOutput())
        repl.waitForExit()
        withClue(s"Maker output was\n${repl.makerOutput()}\n") {
          repl.exitValue() should equal(0)
          findClasses(rootDirectory) should not be (empty)
        }
    }
  }

  "Can compile 2.10 and 2.11 scala versions" in {
    def testCanCompile(scalaVersion: ScalaVersion) {
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

          val scalaVersionText = scalaVersion match {
            case TWO_ELEVEN_DEFAULT => "TWO_ELEVEN_DEFAULT"
            case TWO_TEN_DEFAULT => "TWO_TEN_DEFAULT"
          }
          writeToFile(
            file(rootDirectory, "Project.scala"),
            s"""
              import maker.project._
              import maker.utils.FileUtils._
              import maker.ScalaVersion._

              lazy val a = new Module(
                root = file("$rootDirectory"),
                name = "a",
                scalaVersion = $scalaVersionText
              ) with ClassicLayout 
            
            """
          )

          val repl = TestMakerRepl(rootDirectory)
          findClasses(rootDirectory) should be (empty)
          repl.inputLine("val res = a.compile.succeeded")
          repl.inputLine("System.exit(if (res) 0 else 1)")
          repl.waitForExit()
          withClue(s"Maker output for $scalaVersionText was\n${repl.makerOutput()}\n") {
            repl.exitValue() should equal(0)
            findClasses(rootDirectory) should not be (empty)
          }
      }

    }
    testCanCompile(TWO_ELEVEN_DEFAULT)
    testCanCompile(TWO_TEN_DEFAULT)
  }

  "Compilation across dependent modules works" in {
    withTempDir{
      rootDirectory => 
        writeToFile(
          file(rootDirectory, "a/src/foo/Foo.scala"),
          """
          package foo

          case class Foo(a: Int)
          """
        )
        writeToFile(
          file(rootDirectory, "b/src/bar/Bar.scala"),
          """
          package bar
          import foo.Foo

          case class bar(foo: Foo)
          """
        )

        writeToFile(
          file(rootDirectory, "Project.scala"),
          s"""
            import maker.project._
            import maker.utils.FileUtils._
            import maker.ScalaVersion

            lazy val a = new Module(
              root = file("a"),
              name = "a",
              scalaVersion = ScalaVersion.TWO_ELEVEN_DEFAULT
            ) with ClassicLayout 

            lazy val b = new Module(
              root = file("b"),
              name = "b",
              scalaVersion = ScalaVersion.TWO_ELEVEN_DEFAULT,
              immediateUpstreamModules = Seq(a)
            ) with ClassicLayout 

            lazy val p = new Project(
              name = "project",
              root = file("$rootDirectory"),
              immediateUpstreamModules = Seq(b)
            )
          
          """
        )
        val repl = TestMakerRepl(rootDirectory)
        findClasses(rootDirectory) should be (empty)
        repl.inputLine("val bCompiled = b.compile.succeeded")
        repl.inputLine("p.clean")
        repl.inputLine("val pCompiled = p.compile.succeeded")
        repl.inputLine("System.exit(if (bCompiled && pCompiled) 0 else 1)")
        repl.waitForExit()
        withClue(s"Maker output was\n${repl.makerOutput()}\n") {
          repl.exitValue() should equal(0)
          findClasses(rootDirectory) should not be (empty)
        }

    }
  }

  "strict warnings causes compilation to fail" in {
    def checkCompilation(scalacOptions: Seq[String], expectedExitValue: Int) {
      withTempDir{
        rootDirectory => 

          writeToFile(
            file(rootDirectory, "src", "foo", "Foo.scala"),
            """
            package foo

            object Foo{
              @deprecated("This funciton is deprecated - use something else", "1-1-2015")
              def printEither(x : Either[String, Int]){
                x match {
                  case Left("a") => println(x)
                }
              }
            }
            """
          )

          writeToFile(
            file(rootDirectory, "Project.scala"),
            s"""
              |import maker.project._
              |import maker.utils.FileUtils._
              |import maker.ScalaVersion
              |
              |lazy val a = new Module(
              |  root = file("${rootDirectory}"),
              |  name = "a",
              |  scalaVersion = ScalaVersion.TWO_ELEVEN_DEFAULT
              |) with ClassicLayout { 
              |  override def scalacOptions = Seq(${scalacOptions.mkString("\"", "\", \"", "\"")})
              |}
            
            """.stripMargin
          )

          val repl = TestMakerRepl(rootDirectory)
          findClasses(rootDirectory) should be (empty)
          repl.inputLine("val res = a.compile.succeeded")
          repl.inputLine("System.exit(if (res) 0 else 1)")
          repl.waitForExit()
          withClue(
            repl.makerOutput()
          ) {
            repl.exitValue() should equal(expectedExitValue)
            if (expectedExitValue == 0)
              findClasses(rootDirectory) should not be (empty)
          }
        }
    }

    checkCompilation(Nil, 0)
    checkCompilation(List("-Xfatal-warnings"), 1)
  }
}
