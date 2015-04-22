package maker.project

import maker.utils.FileUtils._
import org.scalatest.{FunSuite, ParallelTestExecution}
import org.slf4j.LoggerFactory

class TopLevelProjectTests extends FunSuite with ParallelTestExecution{

  // Need to create log to get aether to shut up
  val log = LoggerFactory.getLogger(getClass)

  test("Empty top level module"){
    withTempDir{
      dir => 
        val top = Project("You're the top", dir, Nil, isTestProject = true)
        assert(top.compile.succeeded, "Compilation should succeed")
    }
  }

  test("Single module top level module"){
    withTempDir{
      dir => 
        val a = new TestModule(file(dir, "a"), "a") with HasDummyCompiler
        a.writeSrc(
          "foo/Foo.scala", 
          """|package foo
            |
            |case class Foo(a : Int)""".stripMargin
        )
        val top = new Project("Still tops", dir, List(a), isTestProject = true)

        assert(a.compilePhase.classFiles.size == 0, "No class files before compilation")

        assert(top.compile.succeeded, "Compil should succeed")

        assert(a.compilePhase.classFiles.size > 0, "Class files should exist")
    }
  }

  test("Multi module top level module"){
    withTempDir{
      dir => 
        val a = new TestModule(file(dir, "a"), "a") with HasDummyCompiler
        val b = new TestModule(file(dir, "b"), "b", List(a)) with HasDummyCompiler
        a.writeSrc(
          "foo/Foo.scala", 
          """|package foo
             |
             |case class Foo(a : Int)""".stripMargin
        )
        b.writeSrc(
          "bar/Bar.scala", 
          """|package bar
             |
             |import foo.Foo
             |
             |case class Bar(foo : Foo)""".stripMargin
        )
        val top = new Project("Still tops", dir, List(b), isTestProject = true)

        assert(a.compilePhase.classFiles.size == 0, "No class files before compilation")

        assert(top.compile.succeeded, "Compilation should succeed")

        assert(a.compilePhase.classFiles.size > 0, "Class files should exist")
    }
  }
}
