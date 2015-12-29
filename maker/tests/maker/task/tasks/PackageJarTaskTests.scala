package maker.task.tasks

import org.scalatest.{FreeSpec, Matchers, ParallelTestExecution}
import java.io.File
import maker.project._
import maker.utils.{CustomMatchers, Stopwatch, FileUtils}
import maker.task.compile.SourceCompilePhase
import java.util.jar.JarFile
import scala.collection.JavaConversions._
import maker.TestMakerRepl

class PackageJarTaskTests 
  extends FreeSpec with Matchers 
  with ParallelTestExecution
  with FileUtils
{

  "Simple module should package classes and resources" in {
    withTempDir {
      rootDirectory =>  

        writeToFile(
          file(rootDirectory, "src/foo/Foo.scala"),
          """
            package foo

            case class Foo(n : Int)
          """
        )

        TestMakerRepl.writeProjectFile(
          rootDirectory,
          s"""
            lazy val a = new Module(
              root = file("$rootDirectory"),
              name = "a"
            ) with ClassicLayout 

            lazy val p = new Project(
              "p",
              file("$rootDirectory"),
              Seq(a)
            ) 
          """
        )
        val repl = TestMakerRepl(rootDirectory)
        val resourceDir = file(repl.value("a.resourceDir(SourceCompilePhase)"))
        file(resourceDir, "MainResource1").touch
        file(resourceDir, "subdir-b/MainResource2").touch
        repl.value("p.pack.succeeded").toBoolean should be (true)

        val packageJar = file(repl.value("p.packageJar(version = None)"))
        val sourceJar = file(repl.value("p.sourcePackageJar(version = None)"))
        val classDir = file(repl.value("a.classDirectory(SourceCompilePhase)"))
        val sourceDir = file(repl.value("a.sourceDirs(SourceCompilePhase).head"))
        PackageJarTaskTests.checkJarContainsDirectoryContents(
          classDir,
          packageJar
        )
        PackageJarTaskTests.checkJarContainsDirectoryContents(
          resourceDir,
          packageJar
        )
        PackageJarTaskTests.checkJarContainsDirectoryContents(
          sourceDir,
          sourceJar
        )
    }
  }


  "Can package upstream modules into one big jar" in {
    withTempDir{
      rootDirectory => 

        writeToFile(
          file(rootDirectory, "a/src/foo/Foo.scala"),
          """
            package foo
            case object Foo
          """
        )
        writeToFile(
          file(rootDirectory, "b/src/foo/Bar.scala"),
          """
            package foo
            case object Bar
          """
        )
        writeToFile(
          file(rootDirectory, "c/src/foo/Baz.scala"),
          """
            package foo
            case object Baz
          """
        )

        TestMakerRepl.writeProjectFile(
          rootDirectory,
          s"""
            lazy val a = new Module(
              root = file("$rootDirectory", "a"),
              name = "a"
            ) with ClassicLayout 

            lazy val b = new Module(
              root = file("$rootDirectory", "b"),
              name = "b"
            ) with ClassicLayout 

            lazy val c = new Module(
              root = file("$rootDirectory", "c"),
              name = "c",
              compileDependencies = Seq(a)
            ) with ClassicLayout 

            lazy val p = new Project(
              "p",
              file("$rootDirectory"),
              Seq(b, c)
            ) 
          """
        )
        val repl = TestMakerRepl(rootDirectory)

        Seq("a", "b", "c").foreach {
          m =>
            val resourceDir = file(repl.value(s"$m.resourceDir(SourceCompilePhase).getAbsolutePath"))
            file(resourceDir, "${m}-resource").touch
        }

        repl.inputLine("p.pack")

        val oneBigJar = file(repl.value(" p.packageJar(version = None).getAbsolutePath"))
        Seq("a", "b", "c").foreach {
          m => 
            val classDirectory = file(repl.value(s"$m.classDirectory(SourceCompilePhase).getAbsolutePath"))
            PackageJarTaskTests.checkJarContainsDirectoryContents(
              classDirectory,
              oneBigJar
            )
            val resourceDir = file(repl.value(s"$m.resourceDir(SourceCompilePhase).getAbsolutePath"))
            resourceDir.exists should be (true)
            PackageJarTaskTests.checkJarContainsDirectoryContents(
              resourceDir,
              oneBigJar
            )
        }
    }
  }
}

object PackageJarTaskTests extends Matchers with CustomMatchers with FileUtils {
  def checkJarContainsDirectoryContents(dir : File, jarFile : File){
    val jarContents = new JarFile(jarFile).entries.toList.map(_.getName)
    val relativePaths = allFiles(dir).filter(_.isFile).map(_.relativeTo(dir).getPath)

    relativePaths should not be empty // Otherwise why are we testing this

    relativePaths.foreach{
      relPath => 
        assert(jarContents.exists(_.contains(relPath)))
    }
  }
}
