package maker

import org.scalatest.FreeSpec
import maker.utils.FileUtils._

class PomUtilsTests extends FreeSpec {
  // TODO - better XML test
  "test generated file for module" ignore {
    withTempDir{
      rootDirectory => 
        TestMakerRepl.writeProjectFile(
          rootDirectory,
          s"""
            lazy val a = new Module(
              root = file("$rootDirectory", "a"),
              name = "a"
            ) with ClassicLayout {
              override def dependencies = Seq(
                "org.scalatest" % "scalatest" %%  "2.2.0" withScope(TEST)
              )
            }

            lazy val p = new Project(
              name = "project",
              root = file("$rootDirectory"),
              immediateUpstreamModules = Seq(a),
              organization = Some("acme")
            )
          """
        )
        val repl = TestMakerRepl(rootDirectory, teeOutput = true)
        val pomXmlText = repl.value("""maker.PomUtils.pomXml(p, "42", ScalaVersion.TWO_ELEVEN_DEFAULT)""")

        println(pomXmlText)
        assert(
          pomXmlText ===
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

