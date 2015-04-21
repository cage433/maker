package maker

import org.scalatest.FreeSpec
import maker.utils.FileUtils._
import maker.project.TestModule
import maker.project.Project

class PomUtilsTests extends FreeSpec {
  // TODO - better XML test
  "test generated file for module" ignore {
    withTempDir{
      dir => 
        val a = new TestModule(file(dir, "a"), "a")
        val b = new TestModule(file(dir, "b"), "b", List(a))
        val c = new Project("c", dir, List(b))

        assert(
          PomUtils.pomXml(c, "42") ===
          """|<?xml version="1.0" encoding="UTF-8"?>
             |<project xmlns="http://maven.apache.org/POM/4.0.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
             |    xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/maven-v4_0_0.xsd">
             |
             |  <modelVersion>4.0.0</modelVersion>
             |  <groupId>PomUtilsTests</groupId>
             |  <artifactId>c</artifactId>
             |  <packaging>jar</packaging>
             |  <version>42</version>
             |  <dependencies>
             |    <dependency>
             |      <groupId>MakerTestGroupID</groupId>
             |      <artifactId>a</artifactId>
             |      <version>42</version>
             |      <scope>compile</scope>
             |    </dependency>
             |    <dependency>
             |      <groupId>MakerTestGroupID</groupId>
             |      <artifactId>b</artifactId>
             |      <version>42</version>
             |      <scope>compile</scope>
             |    </dependency>
             |  </dependencies>
             |</project>
             |""".stripMargin 
         )
    }

  }
}

