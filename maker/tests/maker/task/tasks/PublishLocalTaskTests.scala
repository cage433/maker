package maker.task.tasks

import org.scalatest.{FreeSpec, Matchers}
import maker.utils.FileUtils._
import maker.project.{TestModule, Project}
import maker.utils.os.{Command, CommandOutputHandler}
import maker.{Resource, MakerProps}
import org.apache.commons.io.{FileUtils => ApacheFileUtils}
import scala.xml.{XML, Node}
import maker.utils.CustomMatchers

class PublishLocalTaskTests extends FreeSpec with Matchers with CustomMatchers{
  
  "Simple module should publish as expected" in {
    withTempDir{
      dir =>  

        val localPublishDir = file(dir, ".publish-local")
        val groupID = "maker-test-group"
        val version = "1.0-SNAPSHOT"
        val props = MakerProps(
          "PublishLocalRootDir", localPublishDir.getAbsolutePath,
          "GroupId", groupID,
          "Compiler", "dummy-test-compiler"
        )
        val proj : TestModule = new TestModule(
          dir,
          "testPublishLocal",
          overrideProps = Some(props)
        )

        // Better to use a jar we know to be in ~/.maker-resource-cache - to 
        // save the download
        writeToFile(
          file(dir, "external-resources"),
          "org.slf4j slf4j-api 1.6.1"
        )

        val managedResources = Vector(
          file(proj.managedResourceDir, "ManagedResource1"), 
          file(proj.managedResourceDir, "subdir", "ManagedResource2")
        ).map(_.touch)

        val mainResources = Vector(
          file(proj.resourceDir, "MainResource1"),
          file(proj.resourceDir, "subdir-b", "MainResource2")
        ).map(_.touch)

        proj.writeSrc(
          "testPublishLocal/Foo.scala",
          """
          |package testPublishLocal
          | 
          |case object Foo
          """.stripMargin
        )

        recursiveDelete(proj.publishLocalDir)
        assert(! proj.publishLocalPomFile.exists, "Published pom should not exist until after publishing")
        assert(! proj.publishLocalJar.exists, "Published jar should not exist until after publishing")

        proj.publishLocal("1.0-SNAPSHOT")

        assert(proj.publishLocalPomFile.exists)

        val pom = XML.loadFile(proj.publishLocalPomFile)

        (pom \ "groupId").text should equal (groupID)
        (pom \ "artifactId").text should equal (proj.name)
        (pom \ "version").text should equal (version)

        val dependencies = pom \\ "dependency"
        val resources = Resource(proj, "org.scala-lang", "scala-library", MakerProps.DefaultScalaVersion) :: proj.resources

        resources should allSatisfy {
          resource : Resource => 
            val resourceCoords = List(resource.groupId, resource.artifactId, resource.version)
            dependencies.exists{
              node => 
                val coords = List("groupId", "artifactId", "version").map{label => (node \ label).text}
                resourceCoords == coords
            }
        }


        assert(proj.publishLocalJar.exists, "Jar should exist")
        val jarContents = {
          val cmd = Command(props.Jar().getAbsolutePath, "tvf", proj.publishLocalJar.getAbsolutePath).withSavedOutput
          cmd.exec
          cmd.savedOutput.split("\n")
        }

        val relativePaths = managedResources.map(_.relativeTo(proj.managedResourceDir).getPath) ++
          mainResources.map(_.relativeTo(proj.resourceDir).getPath) ++
          proj.compilePhase.classFiles.map(_.relativeTo(proj.outputDir).getPath)

        relativePaths should allSatisfy{
          path : String => jarContents.exists(_.contains(path))
        }
    }
  }

  "Top level project should publish as expected" ignore {
    withTempDir {
      dir => 
        val props = TestModule.makeTestProps(dir) ++ (
          "GroupId", "PublishLocalTaskTestsGroup",
          "Compiler", "dummy-test-compiler"
        )
        
        val a = new TestModule(
          file(dir, "a").makeDir,
          "a",
          overrideProps = Some(props)
        )
        val b = new TestModule(
          file(dir, "b").makeDir,
          "b",
          List(a),
          overrideProps = Some(props)
        )
        val topLevel = Project("TopLevelProject", dir, List(b), props)

        topLevel.publishLocal("42")

        assert(topLevel.publishLocalPomFile.exists)

        val expectedTopLevelPomText = 
           """|<?xml version="1.0" encoding="UTF-8"?>
              |<project xmlns="http://maven.apache.org/POM/4.0.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
              |    xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/maven-v4_0_0.xsd">
              |
              |  <modelVersion>4.0.0</modelVersion>
              |  <groupId>PublishLocalTaskTestsGroup</groupId>
              |  <artifactId>TopLevelProject</artifactId>
              |  <packaging>jar</packaging>
              |  <version>42</version>
              |  <dependencies>
              |    <dependency>
              |      <groupId>PublishLocalTaskTestsGroup</groupId>
              |      <artifactId>a</artifactId>
              |      <version>42</version>
              |      <scope>compile</scope>
              |    </dependency>
              |    <dependency>
              |      <groupId>PublishLocalTaskTestsGroup</groupId>
              |      <artifactId>b</artifactId>
              |      <version>42</version>
              |      <scope>compile</scope>
              |    </dependency>
              |  </dependencies>
              |</project>""".stripMargin

        assert(expectedTopLevelPomText === topLevel.publishLocalPomFile.readLines.mkString("\n"))

        val expectedModuleBPomText = 
           """|<?xml version="1.0" encoding="UTF-8"?>
              |<project xmlns="http://maven.apache.org/POM/4.0.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
              |    xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/maven-v4_0_0.xsd">
              |
              |  <modelVersion>4.0.0</modelVersion>
              |  <groupId>PublishLocalTaskTestsGroup</groupId>
              |  <artifactId>b</artifactId>
              |  <packaging>jar</packaging>
              |  <version>42</version>
              |  <dependencies>
              |    <dependency>
              |      <groupId>PublishLocalTaskTestsGroup</groupId>
              |      <artifactId>a</artifactId>
              |      <version>42</version>
              |      <scope>compile</scope>
              |    </dependency>
              |    <dependency>
              |      <groupId>org.scala-lang</groupId>
              |      <artifactId>scala-library</artifactId>
              |      <version>""".stripMargin + MakerProps.DefaultScalaVersion + """</version>
              |      <scope>compile</scope>
              |    </dependency>
              |  </dependencies>
              |</project>""".stripMargin
        assert(expectedModuleBPomText === b.publishLocalPomFile.readLines.mkString("\n"))
    }
  }
}
