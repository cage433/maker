package maker.task.tasks

import org.scalatest.{FreeSpec, Matchers}
import maker.utils.FileUtils._
import maker.project._
import maker.utils.os.{Command, CommandOutputHandler}
import maker._
import org.apache.commons.io.{FileUtils => ApacheFileUtils}
import scala.xml.{XML, Node}
import maker.utils.CustomMatchers
import java.io.File
import scala.collection.immutable.Nil
import maker.task.compile.SourceCompilePhase

class PublishLocalTaskTests extends FreeSpec with Matchers with CustomMatchers{

  private def checkPublishedPomMatchesCoordinates(project : BaseProject, version : String){
    assert(project.publishLocalPomFile(version).exists, s"${project.publishLocalPomFile(version)} doesn't exist")

    val pom = XML.loadFile(project.publishLocalPomFile(version))

    List(
      (project.props.GroupId(), "groupId"), 
      (project.name, "artifactId"),
      (version, "version")
    ).foreach{
      case (expected, label) => 
        assertResult(expected, s"label = $label"){(pom \ label).text}
    }

  }

  private def checkPublishedPomIncludesAllDependencies(module : Module, version : String){
    val pom = XML.loadFile(module.publishLocalPomFile(version))
    val dependencies = pom \\ "dependency"
    val resources = Resource("org.scala-lang", "scala-library", MakerProps.DefaultScalaVersion) :: module.resources

    resources should allSatisfy {
      resource : Resource => 
        val resourceCoords = List(resource.groupId, resource.artifactId, resource.version)
        dependencies.exists{
          node => 
            val coords = List("groupId", "artifactId", "version").map{label => (node \ label).text}
            resourceCoords == coords
        }
    }
  }

  private def createTestModule(dir : File, name : String, upstreamModules : List[Module] = Nil) = {
    val moduleRoot = file(dir, name)
    val props = MakerProps(
      "PublishLocalRootDir", file(moduleRoot, ".publish-local").getAbsolutePath,
      "GroupId",  "maker-test-group",
      "Compiler", "dummy-test-compiler",
      "RunningInMakerTest", "true"
    )
    new TestModule(
      moduleRoot,
      name,
      overrideProps = Some(props),
      upstreamProjects = upstreamModules
    )
  }

  "Simple module should publish as expected" in {
    withTempDir{
      dir =>  

        val version = "1.0-SNAPSHOT"
        val proj = createTestModule(dir, "single-module-publish-local-test")

        proj.addExternalResource("org.slf4j slf4j-api 1.6.1")
        proj.addUnmanagedResource("MainResource1")
        proj.addUnmanagedResource("subdir-b", "MainResource2")

        proj.writeCaseObject("Foo", "testPublishLocal")

        proj.publishLocal(version)

        checkPublishedPomMatchesCoordinates(proj, version)
        checkPublishedPomIncludesAllDependencies(proj, version)
        PackageJarTaskTests.checkJarContainsDirectoryContents(
          proj.outputDir(SourceCompilePhase), proj.publishLocalJar(version))
        PackageJarTaskTests.checkJarContainsDirectoryContents(proj.resourceDir(SourceCompilePhase), proj.publishLocalJar(version))
        PackageJarTaskTests.checkJarContainsDirectoryContents(
          proj.sourceDirs(SourceCompilePhase).head, proj.publishLocalSourceJar(version))
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
        moduleB.publishLocal(version, includeUpstreamModules = true)

        checkPublishedPomMatchesCoordinates(moduleB, version)
        checkPublishedPomIncludesAllDependencies(moduleB, version)

        import moduleB.{publishLocalJar => publishJar, publishLocalSourceJar => publishSourceJar}
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
        val props = TestModule.makeTestProps(dir)
        val version = "1.0-SNAPSHOT"
        val a = createTestModule(dir, "multi-module-publish-local-test-a")
        val b = createTestModule(dir, "multi-module-publish-local-test-b", upstreamModules = List(a))
        val topLevel = Project("TopLevelProject", dir, List(b), props)

        topLevel.publishLocal(version)

        checkPublishedPomMatchesCoordinates(topLevel, version)
        checkPublishedPomMatchesCoordinates(a, version)
        checkPublishedPomMatchesCoordinates(b, version)

    }
  }

}
