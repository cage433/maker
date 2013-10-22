package maker.task.tasks

import org.scalatest.FreeSpec
import maker.utils.FileUtils._
import maker.project.TestModule
import maker.utils.os.Command
import maker.utils.os.CommandOutputHandler
import maker.Resource
import org.apache.commons.io.{FileUtils => ApacheFileUtils}
import maker.project.Project

class PublishLocalTaskTests extends FreeSpec{
  
  "Simple module should publish as expected" in {
    withTempDir{
      dir =>  


        // need a real jar for this test - it doesn't matter which - otherwise
        // the upstream compilation task will fail. Replace it with 
        // any other if utils no longer uses common-io-2.1
        val anyJar = file("utils/lib_managed/commons-io-commons-io-2.1.jar") 

        var resourcesList : List[Resource] = Nil
        val proj : TestModule = new TestModule(
          dir,
          "testPublishLocal",
          overrideProps = Some(TestModule.makeTestProps(dir) ++ ("Compiler", "dummy-test-compiler"))
        ){
          override def resources() = resourcesList
        }
        resourcesList = List(Resource.build(proj, "commons-io commons-io 2.1"))
        proj.managedLibDir.makeDir()
        ApacheFileUtils.copyFileToDirectory(anyJar, proj.managedLibDir)


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

        val expectedPomText = 
           """|<?xml version="1.0" encoding="UTF-8"?>
              |<project xmlns="http://maven.apache.org/POM/4.0.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
              |    xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/maven-v4_0_0.xsd">
              |
              |  <modelVersion>4.0.0</modelVersion>
              |  <groupId>MakerTestGroupID</groupId>
              |  <artifactId>testPublishLocal</artifactId>
              |  <packaging>jar</packaging>
              |  <version>1.0-SNAPSHOT</version>
              |  <dependencies>
              |    <dependency>
              |      <groupId>org.scala-lang</groupId>
              |      <artifactId>scala-library</artifactId>
              |      <version>2.9.2</version>
              |      <scope>compile</scope>
              |    </dependency>
              |    <dependency>
              |      <groupId>commons-io</groupId>
              |      <artifactId>commons-io</artifactId>
              |      <version>2.1</version>
              |      <scope>compile</scope>
              |    </dependency>
              |  </dependencies>
              |</project>""".stripMargin
        val actualPomText = proj.publishLocalPomFile.readLines.mkString("\n")

        assert(actualPomText === expectedPomText)


        assert(proj.publishLocalJar.exists, "Jar should exist")
        val jar = proj.props.Jar().getAbsolutePath
        val jarTfCommand = Command(
          proj.props, 
          CommandOutputHandler.NULL.withSavedOutput,
          None,
          List(jar, "tf", proj.publishLocalJar.getAbsolutePath): _*
        )
        jarTfCommand.exec

        // Order of jar output not deterministic 
        val actualJarLines = jarTfCommand.savedOutput.split("\n").toSet
        val expectedJarLines = 
          """|META-INF/
             |META-INF/MANIFEST.MF
             |testPublishLocal/
             |testPublishLocal/Foo.class
             |""".stripMargin.split("\n").toSet
        assert(expectedJarLines === actualJarLines, "Jar class listing")

    }
  }

  "Top level project should publish as expected" in {
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
              |      <version>2.9.2</version>
              |      <scope>compile</scope>
              |    </dependency>
              |  </dependencies>
              |</project>""".stripMargin
        assert(expectedModuleBPomText === b.publishLocalPomFile.readLines.mkString("\n"))
    }
  }
}
