package maker.project

import org.scalatest.{Matchers, FreeSpec, ParallelTestExecution}
import maker.utils.FileUtils._
import maker.utils.os.Command
import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import xsbti.api.Compilation
import maker.task.compile.SourceCompilePhase
import maker.utils.FileUtils
import maker.TestMakerRepl

class JavaCompileTests 
extends FreeSpec with ParallelTestExecution 
with FileUtils with Matchers {

  "Java module fails when expected and stays failed" in {
    withTempDir{
      rootDirectory => 
        writeToFile(
          file(rootDirectory, "src/foo/Foo.java"), 
          """
          package foo;
          class Foo {
            public int baz() { return 1; }
          }
          """     
        )
        writeToFile(
          file(rootDirectory, "src/foo/Bar.java"), 
          """
          package foo;
          xclass Bar {
            public int baz() { return 1; }
          }
          """     
        )

        TestMakerRepl.writeProjectFile(
          rootDirectory,
          s"""
            lazy val a = new Module(
              root = file("$rootDirectory"),
              name = "a"
            ) with ClassicLayout 
          """
        )
        val repl = TestMakerRepl(rootDirectory)
        findClasses(rootDirectory) should be (empty)

        repl.inputLine("val res = a.compile.succeeded")
        repl.value("res").toBoolean should be (false)
        findClasses(rootDirectory) should be (empty)
        repl.inputLine("clean")

        repl.inputLine("val res = a.compile.succeeded")
        repl.value("res").toBoolean should be (false)

    }
  }

  "Java can compile 1.6 output" in {
     withTempDir{
      rootDirectory => 

        writeToFile(
          file(rootDirectory, "src/foo/Foo.java"), 
          """
          package foo;
          class Foo {
            public int baz() { return 1; }
          }
          """     
        )

        TestMakerRepl.writeProjectFile(
          rootDirectory,
          s"""
            lazy val a = new Module(
              root = file("$rootDirectory"),
              name = "a"
            ) with ClassicLayout  {
              override def javacOptions = List("-source", "1.6", "-target", "1.6")
            }
          """
        )

        val repl = TestMakerRepl(rootDirectory)
        findClasses(rootDirectory) should be (empty)
        repl.inputLine("a.compile")
        findClasses(rootDirectory).size should be (1)
        val classfile = findClasses(rootDirectory).head
        assert(classfile.exists, "Foo.class should exist")

        val bs = new ByteArrayOutputStream()
        val cmd = Command("file", classfile.getAbsolutePath).withOutputTo(bs)
        cmd.run
        assert(bs.toString.contains("compiled Java class data"))
        assert(bs.toString.contains("(Java 1.6)"))

    }
 }
}
