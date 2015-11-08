package maker.task.compile

import org.scalatest.{FunSuite, ParallelTestExecution, Matchers}
import java.io.{File, ByteArrayOutputStream}
import maker.utils.FileUtils._
import maker.project.Module._
import scalaz.syntax.id._
import ch.qos.logback.classic.Level._
import scala.collection.mutable.ListBuffer
import maker.utils.RichString._
import maker.project._
import java.util.concurrent.ConcurrentHashMap
import sbt.inc.Analysis
import com.typesafe.zinc.Compiler
import maker.utils.FileUtils
import maker.TestMaker

class CompileTaskTests extends FunSuite with TestUtils with Matchers with ModuleTestPimps{

  ignore("Can compile a single module project") {
    withTempDir{
      rootDirectory => 
        val testMaker = TestMaker(
          rootDirectory
        ).withModule("a")(rootDirectory)
    }
  }

  ignore("Can compile 2.10 and 2.11 scala versions"){
    withTempDir{
      moduleRoot => 
        TestModuleBuilder.createMakerProjectFile(moduleRoot)
        val module = TestModuleBuilder(
          moduleRoot, 
          "CrossCompiling",
          extraTraits = "org.scalatest.Assertions" :: Nil,
          extraCode = 
             """|
                |import maker.ScalaVersion
                |import maker.task.compile._
                |import scala.language.reflectiveCalls
                |def checkCompilation(scalaVersion : ScalaVersion){
                |
                |  clean(scalaVersion)
                |  assert(classFiles(scalaVersion, SourceCompilePhase).size === 0, s"No class files before $scalaVersion compilation")
                |  println("Compiling")
                |  compile(scalaVersion)
                |  assert(classFiles(scalaVersion, SourceCompilePhase).size > 0, s"Some class files after $scalaVersion compilation")
                |
                |}
                |
                |def checkCrossCompilation{
                |  println("2.10 first")
                |  checkCompilation(ScalaVersion.TWO_TEN_DEFAULT)
                |  checkCompilation(ScalaVersion.TWO_ELEVEN_DEFAULT)
                |}
                |""".stripMargin
        )
        module.appendDefinitionToProjectFile(moduleRoot)
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

        module.writeSrc(
          "foo/bar/Bar.scala", 
          """
          package foo.bar
          import foo.Foo

          case class Bar(x : Foo)
          """
        )

        module.writeSrc(
          "foo/Baz.scala", 
          """
          package foo
          case class Baz(y : Int)
          """
        )
        val result = TestModuleBuilder.makerExecuteCommand(
          moduleRoot,
          "CrossCompiling.checkCrossCompilation"
        ).withNoOutput.run
        result should equal(0)
    }
  }

  def simpleProject(root : File) = {
    val module : TestModule = new TestModule(root, "CompileScalaTaskTests")
    val proj = new Project("CompileScalaTaskTests", root, module :: Nil, isTestProject = true)
    val outputDir = module.classDirectory(SourceCompilePhase)
    val files = new {

      val fooSrc = module.writeSrc(
        "foo/Foo.scala", 
        """
        package foo
        case class Foo(x : Double){
          val fred = 10
          def double() = x + x
        }
        """
      )

      val barSrc = module.writeSrc(
        "foo/bar/Bar.scala", 
        """
        package foo.bar
        import foo.Foo

        case class Bar(x : Foo)
        """
      )

      val bazSrc = module.writeSrc(
        "foo/Baz.scala", 
        """
        package foo
        case class Baz(y : Int)
        """
      )

      def fooClass = new File(outputDir, "foo/Foo.class")
      def fooObject = new File(outputDir, "foo/Foo$.class")
      def barClass = new File(outputDir, "foo/bar/Bar.class")
      def barObject = new File(outputDir, "foo/bar/Bar$.class")
    }
    import files._
    (proj, module, files)
  }

  ignore("Compilation makes class files, writes dependencies, and package makes jar"){
    withTempDir {
      dir => 
        val (proj, module, _) = simpleProject(dir)
        proj.clean
        assert(module.classFiles(SourceCompilePhase).size === 0)
        assert(proj.compile.succeeded, "Compile should succeed")
        assert(module.classFiles(SourceCompilePhase).size > 0)
        assert(!proj.packageJar(version = None).exists)
        proj.pack
        assert(proj.packageJar(version = None).exists)
        proj.clean
        assert(module.classFiles(SourceCompilePhase).size === 0)
        assert(!proj.packageJar(version = None).exists)
    }
  }


  ignore("Deletion of source file causes deletion of class files"){
    withTempDir{
      dir => 
        val (proj, module, files) = simpleProject(dir)
        import files._
        proj.compile
        Set(barClass, barObject) |> {
          s => assert((s & module.classFiles(SourceCompilePhase).toSet) === s)
        }
        assert(barSrc.exists)
        barSrc.delete
        assert(!barSrc.exists)
        sleepToNextSecond
        proj.compile
        assert(!barClass.exists)
        Set(barClass, barObject) |> {
          s => assert((s & module.classFiles(SourceCompilePhase).toSet) === Set())
        }

    }
  }

  ignore("Generated class files are deleted before compilation of source"){
    withTempDir{
      dir => 
        val proj = new TestModule(dir, "CompileScalaTaskTests")
        val fooSrc = file(proj.sourceDirs(SourceCompilePhase).head, "foo/Foo.scala")
        writeToFile(
          fooSrc,
          """
            package foo
            case class Fred(i : Int)
            case class Ginger(i : Int)
          """
        )
        proj.compile
        val fredClass = new File(proj.classDirectory(SourceCompilePhase), "foo/Fred.class")
        val gingerClass = new File(proj.classDirectory(SourceCompilePhase), "foo/Ginger.class")
        assert(fredClass.exists && gingerClass.exists)

        sleepToNextSecond
        writeToFile(
          fooSrc,
          """
            package foo
            case class Fred(i : Int)
            //case class Ginger(i : Int)
          """
        )
        proj.compile
        assert(fredClass.exists, "Fred should still exist")
        assert(!gingerClass.exists, "Ginger should not exist")
    }
  }

  ignore("Recompilation of test source is done if signature of dependent source file changes"){
    withTempDir{
      tempDir => 
        val dir : File = file(tempDir, "proj")
        dir.mkdirs
        
        val proj = new TestModule(dir, "CompileScalaTaskTests")

        proj.writeSrc(
          "foo/Foo.scala",
          """
            package foo
            case class Foo(i : Int){
              def publicMethod{
                println("hi")
              }
            }
          """
        )
        proj.writeTest(
          "foo/FooTest.scala",
          """
            package foo
            class Bar{
              val foo = Foo(20)
              foo.publicMethod
            }
          """
        )
        assert(proj.testCompile.succeeded)
        sleepToNextSecond
        proj.writeSrc(
          "foo/Foo.scala",
          """
            package foo
            case class Foo(i : Int){
              def renamedPublicMethod{
                println("hi")
              }
            }
          """
        )
        val result = proj.testCompile
        assert(!result.succeeded)
    }
  }

  ignore("Compilation across dependent modules works"){
    withTempDir{
      dir => 
        val analyses = new ConcurrentHashMap[File, Analysis]()
        val one = new TestModule(file(dir, "one"), "CompileScalaTaskTests - one", analyses = analyses)
        val two = new TestModule(file(dir, "two"), "CompileScalaTaskTests - two", upstreamProjects = List(one), analyses = analyses)
        
        val fooSrc = one.writeSrc(
          "foo/Foo.scala",
          """
            package foo
            case class Foo(i : Int)
          """
        )
        val barSrc = two.writeSrc(
          "bar/Bar.scala",
          """
            package bar
            import foo.Foo
            case class Bar(foo : Foo){
              val j = foo.i * 2
            }
          """
        )
        assert(two.compile.succeeded)
        sleepToNextSecond

        // Rename variable - module two should now fail to compile
        one.writeSrc(
          "foo/Foo.scala",
          """
            package foo
            case class Foo(j : Int)
          """
        )
        assert(!two.compile.succeeded, "Expected dependent module to fail")
    }
  }

  ignore("When two files are broken fixing one doesn't alow compilation to succeed"){
    withTempDir{
      dir => 
        val proj = new TestModule(dir, "CompileScalaTaskTests")
        
        val fooSrc = file(proj.sourceDirs(SourceCompilePhase).head, "foo/Foo.scala")
        val barSrc = file(proj.sourceDirs(SourceCompilePhase).head, "foo/Bar.scala")
        val bazSrc = file(proj.sourceDirs(SourceCompilePhase).head, "foo/Baz.scala")
        writeToFile(
          fooSrc,
          """
            package foo
            case class Foo(i : Int)
          """
        )
        
        writeToFile(
          barSrc,
          """
            package foo
            case class Bar(foo : Foo)
          """
        )
        writeToFile(
          bazSrc,
          """
            package foo
            case class Baz(foo : Foo)
          """
        )
        assert(proj.compile.succeeded)

        sleepToNextSecond

        writeToFile(
          fooSrc,
          """
            package foo
            case class Foo2(i : Int)
          """
        )
        assert(!proj.compile.succeeded)

        sleepToNextSecond

        writeToFile(
          barSrc,
          """
            package foo
            case class Bar(foo : Foo2)
          """
        )
        assert(!proj.compile.succeeded, "Compilation should have failed")
    }
  }

  /// add test for suspected problem underlying bug #57
  ignore("Compilation across dependent modules and scopes works correctly"){
    withTempDir{
      dir =>
        val analyses = new ConcurrentHashMap[File, Analysis]()
        val one = new TestModule(file(dir, "one"), "one", analyses = analyses)
        val two = new TestModule(file(dir, "two"), "two", upstreamProjects = List(one), analyses = analyses)
        val three = new TestModule(file(dir, "three"), "three", upstreamProjects = List(two), analyses = analyses)


        val fooSrc = one.writeSrc( 
          "foo/Foo.scala",
          """
            package foo
            case class Foo(i : Int)
          """
        )
        val barSrc = two.writeSrc(
          "bar/Bar.scala",
          """
            package bar
            import foo.Foo
            case class Bar(foo : Foo){
              val j = foo.i * 2
            }
          """
        )
        val bazSrc = three.writeSrc(
          "baz/Baz.scala",
          """
            package baz
            import foo.Foo
            import bar.Bar
            case class Baz(bar : Bar, foo : Foo){
              val j = foo.i * 2
            }
          """
        )
        assert(three.compile.succeeded)

        var classes = List(one, two, three).flatMap(_.classFiles(SourceCompilePhase))

        var classCount = classes.groupBy(_.getName)
        assert(classCount.size === 6, "wrong number of generated classes")

        var dups = classCount.filter(_._2.size > 1)
        assert(dups.size == 0, "found duplicates " + dups.map(_._1).mkString(", "))

        // Now we have file dependencies - check that this 
        // doesn't cause duplicates.
        writeToFile(
          fooSrc,
          """
            package foo
            case class Foo(i : Int){
              def twice = 2 * i
            }
          """
        )
        assert(three.compile.succeeded)
        classes = List(one, two, three).flatMap(_.classFiles(SourceCompilePhase))

        classCount = classes.groupBy(_.getName)
        assert(classCount.size === 6, "wrong number of generated classes")

        dups = classCount.filter(_._2.size > 1)
        assert(dups.size == 0, "found duplicates " + dups.map(_._1).mkString(", "))
        
    }
  }

  ignore("Compilation of mutually dependent classes works"){
    withTempDir{
      dir => 
        val proj = new TestModule(dir, "CompileScalaTaskTests")
        val traitSrc = proj.writeSrc(
          "foo/SomeTrait.scala",
          """
package foo

trait SomeTrait{
  self : SomeClass => 
    def baz = bar * 2
}
          """)
        val classSrc = proj.writeSrc(
          "foo/SomeClass.scala",
          """
package foo

class SomeClass extends SomeTrait{
  def bar = 12
}
          """)
          val z = 10


    }
  }


  ignore("Incremental compilation recompiles implementation of changed interfaces"){
    withTempDir{
      dir => 
        val proj = new TestModule(dir, "CompileScalaTaskTests")
        proj.writeSrc(
          "foo/Foo.scala",
          """
          package foo
          trait Foo {
            def bar : Int
          }
          """
        )
        proj.writeSrc(
          "foo/bar/Bar.scala",
          """
          package foo.bar
          import foo.Foo

          class Bar extends Foo {
            def bar : Int = 1
          }
          """
        )
        assert(proj.classFiles(SourceCompilePhase).size === 0)

        assert(proj.compile.succeeded)

        // now update the base trait to invalidate implementations, check it fails
        val compilationTime = proj.lastCompilationTime(SourceCompilePhase).get
        sleepToNextSecond
        proj.writeSrc(
          "foo/Foo.scala",
          """
          package foo
          trait Foo {
            def bar : String
          }
          """
        )


        assert(proj.compile.failed, "compilation succeeded when should have failed")

        var changedClassFiles = proj.classFiles(SourceCompilePhase).filter(_.lastModified >= compilationTime)
        val fooClass = file(proj.classDirectory(SourceCompilePhase), "foo", "Foo.class")
        val barClass = file(proj.classDirectory(SourceCompilePhase), "foo", "bar", "Bar.class")

        // Apparently the new incremental compiler doesn't create class files if there is any failure
        // NOT TRUE ANYMORE
        // assert(changedClassFiles === Set())

        // now put a matching implementation in and all should be ok again
        proj.writeSrc(
          "foo/bar/Bar.scala",
          """
          package foo.bar
          import foo.Foo
          class Bar extends Foo {
            def bar : String = "1"
          }
          """
        )

        assert(proj.compile.succeeded, "compilation failed when should have succeeded")

        changedClassFiles = proj.classFiles(SourceCompilePhase).filter(_.lastModified >= compilationTime)
        assert(Set.empty ++ changedClassFiles === Set(fooClass, barClass))
    }
  }

  ignore("Adding parameter to constructor causes recompilation of downstream file"){
    withTempDir{
      dir => 
        val analyses = new ConcurrentHashMap[File, Analysis]()
        val A = new TestModule(file(dir, "A"), "A", analyses = analyses)
        val B = new TestModule(file(dir, "B"), "B", List(A), analyses = analyses)
        A.writeSrc(
          "foo/Foo.scala",
          """
          package foo
          case class Foo(x : Int)
          """
        )
        B.writeSrc(
          "bar/Bar.scala",
          """
          import foo.Foo

          case class Bar(x : Int){
            val foo = Foo(3)
          }
          """
        )
        assert(B.compile.succeeded)

        sleepToNextSecond
        A.writeSrc(
          "foo/Foo.scala",
          """
          package foo
          case class Foo(x : Int, y : Int)
          """
        )

        assert(B.compile.failed, "Expected compilation to fail")
    }
  }

  ignore("strict warnings causes compilation to fail"){
    withTempDir{
      dir => 
        val a = new TestModule(dir, "a"){
          override def scalacOptions = List("-unchecked", "-deprecation", "-feature", "-Xfatal-warnings")
        }
        a.writeSrc(
          "foo/Foo.scala",
          """
          package foo

          object Foo{
            def printEither(x : Either[String, Int]){
              x match {
                case Left("a") => println(x)
              }
            }
          }
          """
        )
        assert(a.compile.failed)
    }
  }
}
