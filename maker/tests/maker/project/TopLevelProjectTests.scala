package maker.project

import maker.utils.FileUtils._
import org.scalatest.FunSuite
import org.scalatest.ParallelTestExecution
import maker.Props

class TopLevelProjectTests extends FunSuite with ParallelTestExecution{

  test("Empty top level module"){
    withTempDir{
      dir => 
        val props = Props.initialiseTestProps(dir, "Compiler", "dummy-test-compiler")
        val top = Project("You're the top", dir, Nil, props)

        assert(top.compile.succeeded, "Compilation should succeed")
    }
  }

  test("Single module top level module"){
    withTempDir{
      dir => 
        val props = Props.initialiseTestProps(dir, "Compiler", "dummy-test-compiler")
        val a = TestModule(file(dir, "a"), "a", props)
        a.writeSrc(
          "foo/Foo.scala", 
          """|package foo
            |
            |case class Foo(a : Int)""".stripMargin
        )
        val top = new Project("Still tops", dir, List(a), props = props)

        assert(a.compilePhase.classFiles.size == 0, "No class files before compilation")

        assert(top.compile.succeeded, "Compil should succeed")

        assert(a.compilePhase.classFiles.size > 0, "Class files should exist")
    }
  }

  test("Multi module top level module"){
    withTempDir{
      dir => 
        val props = Props.initialiseTestProps(dir, "Compiler", "dummy-test-compiler")
        val a = TestModule(file(dir, "a"), "a", props)
        val b = TestModule(file(dir, "b"), "b", props, List(a))
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
        val top = new Project("Still tops", dir, List(b), props = props)

        assert(a.compilePhase.classFiles.size == 0, "No class files before compilation")

        assert(top.compile.succeeded, "Compilation should succeed")

        assert(a.compilePhase.classFiles.size > 0, "Class files should exist")
    }
  }
}
