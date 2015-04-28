package maker.project

import org.scalatest.FunSuite
import maker.utils.FileUtils._
import maker.utils.os.Command
import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import xsbti.api.Compilation
import maker.task.compile.SourceCompilePhase

class JavaCompileTests extends FunSuite with TestUtils with ModuleTestPimps{

  test("Java module fails when expected and stays failed"){
    withTempDir{
      root => 
        val module = new TestModule(root, "JavaCompileTests")
        module.writeSrc(
          "src/foo/Foo.java", 
          """
          package foo;
          class Foo {
            public int baz() { return 1; }
          }
          """     
        )
        module.writeSrc(
          "src/foo/Bar.java", 
          """
          package foo;
          xclass Bar {
            public int baz() { return 1; }
          }
          """     
        )


        module.clean
        assert(module.classFiles(SourceCompilePhase).size === 0)

        assert(module.compile.failed, "Compilation should have failed")

        assert(!file(module.classDirectory(SourceCompilePhase), "foo", "Bar.class").exists, "Bar.class should not exist")
        sleepToNextSecond
        assert(module.compile.failed, "Compilation should have failed")
    }
  }

  test("Java can compile 1.6 output"){
     withTempDir{
      root => 
        val module = new TestModule(root, "JavaCompileTests"){
          override def javacOptions = List("-source", "1.6", "-target", "1.6")
        }
        module.writeSrc(
          "src/foo/Foo.java", 
          """
          package foo;
          class Foo {
            public int baz() { return 1; }
          }
          """     
        )


        module.compile
        assert(module.classFiles(SourceCompilePhase).size === 1)
        val classfile = file(module.classDirectory(SourceCompilePhase), "foo", "Foo.class")
        assert(classfile.exists, "Foo.class should exist")

        val bs = new ByteArrayOutputStream()
        val cmd = Command("file", classfile.getAbsolutePath).withOutputTo(bs)
        cmd.run
        assert(bs.toString.contains("compiled Java class data"))
        assert(bs.toString.contains("(Java 1.6)"))

    }
 }
}
