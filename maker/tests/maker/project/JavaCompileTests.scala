package maker.project

import org.scalatest.FunSuite
import maker.utils.FileUtils._
import maker.utils.os.Command

class JavaCompileTests extends FunSuite with TestUtils {

  test("Java module fails when expected and stays failed"){
    withTempDir{
      root => 
        val proj = new TestModule(root, "JavaCompileTests")
        proj.writeSrc(
          "src/foo/Foo.java", 
          """
          package foo;
          class Foo {
            public int baz() { return 1; }
          }
          """     
        )
        proj.writeSrc(
          "src/foo/Bar.java", 
          """
          package foo;
          xclass Bar {
            public int baz() { return 1; }
          }
          """     
        )


        proj.clean
        assert(proj.compilePhase.classFiles.size === 0)

        assert(proj.compile.failed, "Compilation should have failed")

        assert(!file(proj.compilePhase.outputDir, "foo", "Bar.class").exists, "Bar.class should not exist")
        sleepToNextSecond
        assert(proj.compile.failed, "Compilation should have failed")
    }
  }

  test("Java can compile 1.6 output"){
     withTempDir{
      root => 
        val proj = new TestModule(root, "JavaCompileTests"){
          override def javacOptions = List("-source", "1.6", "-target", "1.6")
        }
        proj.writeSrc(
          "src/foo/Foo.java", 
          """
          package foo;
          class Foo {
            public int baz() { return 1; }
          }
          """     
        )


        proj.compile
        assert(proj.compilePhase.classFiles.size === 1)
        val classfile = file(proj.compilePhase.outputDir, "foo", "Foo.class")
        assert(classfile.exists, "Foo.class should exist")

        val cmd = Command("file", classfile.getAbsolutePath).withSavedOutput
        cmd.exec
        assert(cmd.savedOutput.contains("compiled Java class data"))
        assert(cmd.savedOutput.contains("(Java 1.6)"))

    }
 }
}
