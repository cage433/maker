package maker.task.tasks

import org.scalatest.{FreeSpec, Matchers}
import maker.utils.os.Command
import java.io.File
import maker.project.{Module, TestModule, HasDummyCompiler}
import maker.utils.FileUtils._
import maker.utils.{CustomMatchers, Stopwatch}
import maker.MakerProps
import maker.task.compile.SourceCompilePhase
import java.util.jar.JarFile
import scala.collection.JavaConversions._

class PackageJarTaskTests extends FreeSpec with Matchers{
  private def createTestModule(dir : File, name : String, upstreamModules : List[Module] = Nil) = {
    val moduleRoot = file(dir, name)
    new TestModule(
      moduleRoot,
      name,
      upstreamProjects = upstreamModules
    ) with HasDummyCompiler
  }

  "Simple module should package classes and resources" in {
    withTempDir{
      dir =>  

        val proj = createTestModule(dir, "single-module-package-jar-test")

        proj.addUnmanagedResource("MainResource1")
        proj.addUnmanagedResource("subdir-b", "MainResource2")

        proj.writeCaseObject("Foo", "foo")

        proj.pack().succeeded should be (true)

        PackageJarTaskTests.checkJarContainsDirectoryContents(
          proj.outputDir(SourceCompilePhase), 
          proj.packageJar(SourceCompilePhase, version = None))
        PackageJarTaskTests.checkJarContainsDirectoryContents(
          proj.resourceDir(SourceCompilePhase), 
          proj.packageJar(SourceCompilePhase, version = None))
    }
  }

  "Simple module should package source" in {
    withTempDir{
      dir =>  

        val proj = createTestModule(dir, "single-module-package-jar-test")

        proj.addUnmanagedResource("MainResource1")
        proj.addUnmanagedResource("subdir-b", "MainResource2")

        proj.writeCaseObject("Foo", "foo")

        proj.pack()

        PackageJarTaskTests.checkJarContainsDirectoryContents(
          proj.sourceDirs(SourceCompilePhase).head,
          proj.sourcePackageJar(SourceCompilePhase, version = None))
    }
  }
  "Can package upstream modules into one big jar" in {
    withTempDir{
      dir => 
        val a = createTestModule(dir, "a")
        val b = createTestModule(dir, "b", upstreamModules = List(a))
        val c = createTestModule(dir, "c", upstreamModules = List(b))


        a.writeCaseObject("Foo", "foo")
        a.addUnmanagedResource("a-resource")

        b.writeCaseObject("Bar", "foo")
        b.addUnmanagedResource("b-resource")

        c.writeCaseObject("Baz", "foo")
        c.addUnmanagedResource("c-resource")

        c.pack(includeUpstreamModules = true)

        val oneBigJar = c.packageJar(SourceCompilePhase, version = None)

        Vector(a, b, c).foreach{
          m => 
            PackageJarTaskTests.checkJarContainsDirectoryContents(
              m.outputDir(SourceCompilePhase),
              oneBigJar
            )
            PackageJarTaskTests.checkJarContainsDirectoryContents(
              m.resourceDir(SourceCompilePhase),
              oneBigJar
            )
        }
    }
  }
}

object PackageJarTaskTests extends Matchers with CustomMatchers{
  def checkJarContainsDirectoryContents(dir : File, jarFile : File){
    val jarContents = new JarFile(jarFile).entries.toList.map(_.getName)
    val relativePaths = allFiles(dir).filter(_.isFile).map(_.relativeTo(dir).getPath)

    relativePaths should not be empty // Otherwise why are we testing this

    relativePaths.foreach{
      relPath => 
        if (! jarContents.exists(_.contains(relPath))){
          println(s"Could not find $relPath in $jarFile")
          jarContents.foreach(println)
          fail(s"Could not find $relPath in $jarFile")
        }
    }
  }
}
