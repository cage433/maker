package maker.task.publish

import org.scalatest.FreeSpec
import maker.utils.FileUtils._
import maker.project.TestModule
import maker.task.update.Resource._
import maker.task.update.Resource
import org.apache.commons.io.{FileUtils => ApacheFileUtils}
import maker.project.Project
import maker.Props
import maker.utils.Implicits.RichString._

class PublishTaskTests extends FreeSpec {
  "test whether dynamic ivy is really needed" in {
    withTestDir{
      dir =>  

        val publishDir = file(dir, "publish").makeDir
        val ivySettingsFile_ = file(dir, "ivysettings.xml")
        writeToFile(
          ivySettingsFile_,
          """ |<ivysettings>
              |  <property name="ivy.default.conf.mappings" value="default->*" />
              |  <resolvers>
              |    <filesystem name="maker-local" m2compatible="true">
              |      <artifact pattern="%s/[module]/[revision]/[artifact]-[revision].[ext]" />
              |    </filesystem>
              |  </resolvers>
              |</ivysettings>""".stripMargin % publishDir)

        val proj = TestModule(dir, "testPublish", 
          Props.initialiseTestProps(dir) ++ (
            "Compiler", "dummy-test-compiler",
            "IvySettingsFile", ivySettingsFile_.absPath
          ))
        // Not needed as we aren't running tests - and ivy will just try to get them
        file(dir, "external-resources").delete

        proj.writeSrc(
          "testPublish/Foo.scala",
          """
          |package testPublish
          | 
          |case object Foo
          """.stripMargin
        )
        proj.publish("1.0-SNAPSHOT", "maker-local")

        val expectedPomText = 
           """|<?xml version="1.0" encoding="UTF-8"?>
              |<project xmlns="http://maven.apache.org/POM/4.0.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
              |    xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/maven-v4_0_0.xsd">
              |
              |  <modelVersion>4.0.0</modelVersion>
              |  <groupId>MakerTestGroupID</groupId>
              |  <artifactId>testPublish</artifactId>
              |  <packaging>jar</packaging>
              |  <version>1.0-SNAPSHOT</version>
              |  <dependencies>
              |    <dependency>
              |      <groupId>org.scala-lang</groupId>
              |      <artifactId>scala-library</artifactId>
              |      <version>2.10.2</version>
              |      <scope>compile</scope>
              |    </dependency>
              |  </dependencies>
              |</project>""".stripMargin
        val publishedPomFile = file(dir, "publish-local/testPublish/1.0-SNAPSHOT/testPublish-1.0-SNAPSHOT.pom")
        val actualPomText = proj.publishLocalPomFile.readLines.mkString("\n")
        assert(expectedPomText === actualPomText)


    }

  }
}
