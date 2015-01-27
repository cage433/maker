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
          IvyUtils.generateIvyFile(b, "0.1").readLines.mkString("\n") === 
          """|<ivy-module version="1.0" xmlns:e="http://ant.apache.org/ivy/extra">
             |  <info organisation="MakerTestGroupID" module="b" revision="0.1"/>
             |  <configurations>
             |    <conf name="default" transitive="false"/>
             |    <conf name="compile" transitive="false"/>
             |    <conf name="test" transitive="false"/>
             |  </configurations>
             |  <publications>
             |    <artifact name="b" type="pom"/>
             |    <artifact name="b" type="jar" ext="jar" conf="default"/>
             |    <artifact name="b" type="jar" ext="jar" e:classifier="sources" conf="default"/>
             |    <artifact name="b" type="jar" ext="jar" e:classifier="javadoc" conf="default"/>
             |  </publications>
             |  <dependencies defaultconfmapping="${ivy.default.conf.mappings}">
             |    <exclude org="MakerTestGroupID" module="a"/>
             |    <exclude org="MakerTestGroupID" module="b"/>
             |  </dependencies>
             |</ivy-module>""".stripMargin
        )

        assert(
          IvyUtils.generateIvyFile(c, "0.1").readLines.mkString("\n") === 
          """|<ivy-module version="1.0" xmlns:e="http://ant.apache.org/ivy/extra">
             |  <info organisation="MakerTestGroupID" module="c" revision="0.1"/>
             |  <configurations>
             |    <conf name="default" transitive="false"/>
             |    <conf name="compile" transitive="false"/>
             |    <conf name="test" transitive="false"/>
             |  </configurations>
             |  <publications>
             |    <artifact name="c" type="pom"/>
             |    <artifact name="c" type="jar" ext="jar" conf="default"/>
             |    <artifact name="c" type="jar" ext="jar" e:classifier="sources" conf="default"/>
             |    <artifact name="c" type="jar" ext="jar" e:classifier="javadoc" conf="default"/>
             |  </publications>
             |  <dependencies defaultconfmapping="${ivy.default.conf.mappings}">
             |    <exclude org="MakerTestGroupID" module="a"/>
             |    <exclude org="MakerTestGroupID" module="b"/>
             |    <exclude org="MakerTestGroupID" module="c"/>
             |  </dependencies>
             |</ivy-module>""".stripMargin
        )
    }
  }
}
