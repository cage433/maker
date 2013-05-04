package maker.project

import org.scalatest.FunSuite
import java.io.File
import maker.utils.FileUtils._
import maker.project.Project._
import maker.MakerProps
import maker.task.compile._


class JavaCompileTests extends FunSuite with TestUtils {

  test("Java project fails when expected and stays failed"){
    withTempDir{
      root ⇒ 
        val proj = new TestProject(root, "JavaCompileTests", props = MakerProps("MakerLogLevel", "ERROR", "ShowCompilerOutput", "false"))
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

        //assert(files.fooClass.exists, "failed to create Foo.class")
        assert(!file(proj.compilePhase.outputDir, "foo", "Bar.class").exists, "Bar.class should not exist")
        sleepToNextSecond
        assert(proj.compile.failed, "Compilation should have failed")
    }
  }
}
