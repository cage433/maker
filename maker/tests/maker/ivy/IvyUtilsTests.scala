package maker.ivy

import org.scalatest.FreeSpec
import maker.utils.FileUtils._
import maker.project.TestModule
import maker.project.Project

class IvyUtilsTests extends FreeSpec {
  "test dynamic ivy file" in {
    withTempDir{
      dir => 
        val a = new TestModule(file(dir, "a").makeDir, "a"){
          override def ivySettingsFile = file(dir, "ivysettings.xml")
        }
        val b = new TestModule(file(dir, "b").makeDir, "b", List(a)){
          override def ivySettingsFile = file(dir, "ivysettings.xml")
        }
        val c = new Project("c", dir, List(a, b), TestModule.makeTestProps(dir)){
          override def ivySettingsFile = file(dir, "ivysettings.xml")
        }

        
        assert(
          IvyUtils.generateIvyFile(b).readLines.mkString("\n") === 
          """|<ivy-module version="1.0" xmlns:e="http://ant.apache.org/ivy/extra">
             |  <info revision="${maker.module.version}" module="b" organisation="MakerTestGroupID"></info>
             |  <configurations>
             |    <conf name="default" transitive="false"></conf>
             |    <conf name="compile" transitive="false"></conf>
             |    <conf name="test" transitive="false"></conf>
             |  </configurations>
             |  <publications>
             |    <artifact type="pom" name="b"></artifact>
             |    <artifact conf="default" type="jar" name="b" ext="jar"></artifact>
             |  </publications>
             |  <dependencies defaultconfmapping="${ivy.default.conf.mappings}">
             |    <exclude org="MakerTestGroupID" module="a"></exclude>
             |    <exclude org="MakerTestGroupID" module="b"></exclude>
             |  </dependencies>
             |</ivy-module>""".stripMargin
        )

        assert(
          IvyUtils.generateIvyFile(c).readLines.mkString("\n") === 
          """|<ivy-module version="1.0" xmlns:e="http://ant.apache.org/ivy/extra">
             |  <info revision="${maker.module.version}" module="c" organisation="MakerTestGroupID"></info>
             |  <configurations>
             |    <conf name="default" transitive="false"></conf>
             |    <conf name="compile" transitive="false"></conf>
             |    <conf name="test" transitive="false"></conf>
             |  </configurations>
             |  <publications>
             |    <artifact type="pom" name="c"></artifact>
             |    <artifact conf="default" type="jar" name="c" ext="jar"></artifact>
             |  </publications>
             |  <dependencies defaultconfmapping="${ivy.default.conf.mappings}">
             |    <exclude org="MakerTestGroupID" module="a"></exclude>
             |    <exclude org="MakerTestGroupID" module="b"></exclude>
             |    <exclude org="MakerTestGroupID" module="c"></exclude>
             |  </dependencies>
             |</ivy-module>""".stripMargin
        )
    }
  }
}
