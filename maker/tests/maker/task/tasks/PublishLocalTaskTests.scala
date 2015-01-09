package maker.task.tasks

import org.scalatest.{FreeSpec, Matchers}
import maker.utils.FileUtils._
import maker.project._
import maker.utils.os.{Command, CommandOutputHandler}
import maker.{Resource, MakerProps}
import org.apache.commons.io.{FileUtils => ApacheFileUtils}
import scala.xml.{XML, Node}
import maker.utils.CustomMatchers
import java.io.File

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

  private def checkJarContainsDirectoryContents(module : Module, dir : File){
    val jarContents = {
      val cmd = Command(module.props.Jar().getAbsolutePath, "tvf", module.publishLocalJar.getAbsolutePath).withSavedOutput
      cmd.exec
      cmd.savedOutput.split("\n")
    }
    val relativePaths = allFiles(dir).filter(_.isFile).map(_.relativeTo(dir).getPath)
    relativePaths should allSatisfy{
      path : String => jarContents.exists(_.contains(path))
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

  private def addExternalResource(module : Module, resource : String){
    appendToFile(file(module.root, "external-resources"), resource)
  }

  private def addUnmanagedResource(module : Module, filePath : String*){
    file(module.resourceDir, filePath :_*).touch
  }

  private def writeSrc(module : Module, path : String*){
    val pckg = path.dropRight(1).mkString(".")
    val relativePath = path.mkString("/")
  }


  "Simple module should publish as expected" in {
    withTempDir{
      dir =>  

        val version = "1.0-SNAPSHOT"
        val proj = createTestModule(dir, "single-module-publish-local-test")

        addExternalResource(proj, "org.slf4j slf4j-api 1.6.1")
        addUnmanagedResource(proj, "MainResource1")
        addUnmanagedResource(proj, "subdir-b", "MainResource2")

        proj.writeSrc(
          "testPublishLocal/Foo.scala",
          """
          |package testPublishLocal
          | 
          |case object Foo
          """.stripMargin
        )

        proj.publishLocal(version)

        checkPublishedPomMatchesCoordinates(proj, version)
        checkPublishedPomIncludesAllDependencies(proj)
        checkJarContainsDirectoryContents(proj, proj.outputDir)
        checkJarContainsDirectoryContents(proj, proj.resourceDir)
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
