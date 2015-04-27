package maker.task.tasks

import org.scalatest.{FreeSpec, Matchers}
import maker.utils.FileUtils._
import maker.project._
import maker._
import org.apache.commons.io.{FileUtils => ApacheFileUtils}
import scala.xml.{XML, Node}
import maker.utils.CustomMatchers
import java.io.File
import scala.collection.immutable.Nil
import maker.task.compile.SourceCompilePhase
import org.eclipse.aether.graph.{Dependency => AetherDependency}

class PublishLocalTaskTests 
  extends FreeSpec 
  with Matchers 
  with CustomMatchers
  with ConfigPimps
  with DependencyPimps
{

  private def checkPublishedPomMatchesCoordinates(project : Project, version : String){
    assert(project.publishLocalPomFile(version).exists, s"${project.publishLocalPomFile(version)} doesn't exist")

    val pom = XML.loadFile(project.publishLocalPomFile(version))

    List(
      (project.organization.getOrElse(???), "groupId"), 
      (project.name, "artifactId"),
      (version, "version")
    ).foreach{
      case (expected, label) => 
        assertResult(expected, s"label = $label"){(pom \ label).text}
    }

  }

  private def checkPublishedPomIncludesAllDependencies(project : Project, version : String){
    import project.config
    val pom = XML.loadFile(project.publishLocalPomFile(version))
    val pomDependencies = pom \\ "dependency"
    val dependencies = config.scalaVersion.scalaLibraryResource +: project.dependencies

    dependencies should allSatisfy {
      dependency : AetherDependency => 
        val coords = List(dependency.groupId, dependency.artifactId, dependency.version)
        pomDependencies.exists{
          node => 
            val pomCoords = List("groupId", "artifactId", "version").map{label => (node \ label).text}
            coords == pomCoords
        }
    }
  }

  private def createTestModule(dir : File, name : String, upstreamModules : List[Module] = Nil) = {
    val moduleRoot = file(dir, name)
    new TestModule(
      moduleRoot,
      name,
      upstreamProjects = upstreamModules
    ) with HasDummyCompiler
  }

  "Simple module should publish as expected" in {
    withTempDir{
      dir =>  

        val version = "1.0-SNAPSHOT"
        val module = createTestModule(dir, "single-module-publish-local-test")
        val proj = new Project(module.name, dir, module :: Nil, isTestProject = true){
          override def organization = Some("org.org")
        }

        module.addExternalResource("org.slf4j slf4j-api 1.6.1")
        module.addUnmanagedResource("MainResource1")
        module.addUnmanagedResource("subdir-b", "MainResource2")

        module.writeCaseObject("Foo", "testPublishLocal")

        proj.publishLocal(version)

        checkPublishedPomMatchesCoordinates(proj, version)
        checkPublishedPomIncludesAllDependencies(proj, version)
        PackageJarTaskTests.checkJarContainsDirectoryContents(
          module.outputDir(SourceCompilePhase), proj.publishLocalJar(version))
        PackageJarTaskTests.checkJarContainsDirectoryContents(module.resourceDir(SourceCompilePhase), proj.publishLocalJar(version))
        PackageJarTaskTests.checkJarContainsDirectoryContents(
          module.sourceDirs(SourceCompilePhase).head, proj.publishLocalSourceJar(version))
    }
  }

  "Module can publish itself and dependencies as a single artifact" in {
    withTempDir{
      dir =>  

        import PackageJarTaskTests.checkJarContainsDirectoryContents

        val version = "1.0-SNAPSHOT"
        val moduleA = {
          val proj = createTestModule(dir, "A")
          proj.addExternalResource("org.slf4j slf4j-api 1.6.1")
          proj.addUnmanagedResource("MainResource1")
          proj.addUnmanagedResource("subdir-a", "MainResource2")
          proj.writeCaseObject("Foo", "foo")
          proj
        }

        val moduleB = {
          val proj = createTestModule(dir, "B", List(moduleA))
          proj.addExternalResource("org.slf4j slf4j-api 1.6.1")
          proj.addUnmanagedResource("MainResource3")
          proj.addUnmanagedResource("subdir-b", "MainResource4")
          proj.writeCaseObject("Bar", "bar")
          proj
        }
        val project = new Project("publish test", dir, moduleA :: moduleB :: Nil, isTestProject = true){
          override def organization = Some("org.org")
        }
        project.publishLocal(version)

        checkPublishedPomMatchesCoordinates(project, version)
        checkPublishedPomIncludesAllDependencies(project, version)

        import project.{publishLocalJar => publishJar, publishLocalSourceJar => publishSourceJar}
        publishJar(version) should be ('exists)

        Vector(moduleA, moduleB).foreach{
          module => 
            checkJarContainsDirectoryContents(
              module.outputDir(SourceCompilePhase), publishJar(version))
            checkJarContainsDirectoryContents(module.resourceDir(SourceCompilePhase), publishJar(version))
            checkJarContainsDirectoryContents(
              module.sourceDirs(SourceCompilePhase).head, publishSourceJar(version))
        }
    }
  }
  "Top level project should publish each sub module" in {
    withTempDir {
      dir => 
        val version = "1.0-SNAPSHOT"
        val a = createTestModule(dir, "multi-module-publish-local-test-a")
        val b = createTestModule(dir, "multi-module-publish-local-test-b", upstreamModules = List(a))
        val topLevel =new  Project("TopLevelProject", dir, List(b), isTestProject = true){
          override def organization = Some("org.org")
        }

        topLevel.publishLocal(version)

        checkPublishedPomMatchesCoordinates(topLevel, version)

    }
  }

}
