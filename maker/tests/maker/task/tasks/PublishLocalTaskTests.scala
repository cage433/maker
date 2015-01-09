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
    assert(project.publishLocalPomFile.exists)

    val pom = XML.loadFile(project.publishLocalPomFile)

    List(
      (project.props.GroupId(), "groupId"), 
      (project.name, "artifactId"),
      (version, "version")
    ).foreach{
      case (expected, label) => 
        assertResult(expected, s"label = $label"){(pom \ label).text}
    }

  }

  private def checkPublishedPomIncludesAllDependencies(module : Module){
    val pom = XML.loadFile(module.publishLocalPomFile)
    val dependencies = pom \\ "dependency"
    val resources = Resource(module, "org.scala-lang", "scala-library", MakerProps.DefaultScalaVersion) :: module.resources

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
      "Compiler", "dummy-test-compiler"
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
        checkPublishedPomIncludesAllDependencies(proj)
        PackageJarTaskTests.checkJarContainsDirectoryContents(
          proj, proj.outputDir(SourceCompilePhase), proj.publishLocalJar)
        PackageJarTaskTests.checkJarContainsDirectoryContents(proj, proj.resourceDir(SourceCompilePhase), proj.publishLocalJar)
    }
  }

  "Top level project should publish each sub module" in {
    withTempDir {
      dir => 
        val version = "1.0-SNAPSHOT"
        val a = createTestModule(dir, "multi-module-publish-local-test-a")
        val b = createTestModule(dir, "multi-module-publish-local-test-b", upstreamModules = List(a))
        val topLevel = Project("TopLevelProject", dir, List(b), a.props)

        topLevel.publishLocal(version)

        checkPublishedPomMatchesCoordinates(topLevel, version)
        checkPublishedPomMatchesCoordinates(a, version)
        checkPublishedPomMatchesCoordinates(b, version)

    }
  }
}
