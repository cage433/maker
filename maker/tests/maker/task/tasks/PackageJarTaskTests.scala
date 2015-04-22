package maker.task.tasks

import org.scalatest.{FreeSpec, Matchers}
import java.io.File
import maker.project._
import maker.utils.FileUtils._
import maker.utils.{CustomMatchers, Stopwatch}
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

        val module = createTestModule(dir, "single-module-package-jar-test")
        val proj = Project(module.name, dir, module :: Nil, isTestProject = true)

        module.addUnmanagedResource("MainResource1")
        module.addUnmanagedResource("subdir-b", "MainResource2")

        module.writeCaseObject("Foo", "foo")

        proj.pack.succeeded should be (true)

        PackageJarTaskTests.checkJarContainsDirectoryContents(
          module.outputDir(SourceCompilePhase), 
          proj.packageJar(version = None))
        PackageJarTaskTests.checkJarContainsDirectoryContents(
          module.resourceDir(SourceCompilePhase), 
          proj.packageJar(version = None))
    }
  }

  "Simple module should package source" in {
    withTempDir{
      dir =>  

        val module = createTestModule(dir, "single-module-package-jar-test")
        val proj = Project(module.name, dir, module :: Nil, isTestProject = true)

        module.addUnmanagedResource("MainResource1")
        module.addUnmanagedResource("subdir-b", "MainResource2")

        module.writeCaseObject("Foo", "foo")

        proj.pack

        PackageJarTaskTests.checkJarContainsDirectoryContents(
          module.sourceDirs(SourceCompilePhase).head,
          proj.sourcePackageJar(version = None))
    }
  }
  "Can package upstream modules into one big jar" in {
    withTempDir{
      dir => 
        val a = createTestModule(dir, "a")
        val b = createTestModule(dir, "b", upstreamModules = List(a))
        val c = createTestModule(dir, "c", upstreamModules = List(b))
        val p = Project("p", dir, a :: b :: c :: Nil, isTestProject = true)


        a.writeCaseObject("Foo", "foo")
        a.addUnmanagedResource("a-resource")

        b.writeCaseObject("Bar", "foo")
        b.addUnmanagedResource("b-resource")

        c.writeCaseObject("Baz", "foo")
        c.addUnmanagedResource("c-resource")

        p.pack

        val oneBigJar = p.packageJar(version = None)

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
        assert(jarContents.exists(_.contains(relPath)))
    }
  }
}
