package maker.task.publish

import org.scalatest.FreeSpec
import maker.utils.FileUtils._
import maker.project.TestModule
import maker.utils.Implicits.RichString._
import maker.project.Project
import maker.Props

class IvyUtilsTests extends FreeSpec {
  "test dynamic ivy file" in {
    withTempDir{
      dir => 
        val props = Props.initialiseTestProps(dir)
        val a = TestModule(file(dir, "a").makeDir, "a", props)
        val b = TestModule(file(dir, "b").makeDir, "b", props, List(a))
        val c = new Project("c", dir, List(a, b), props)

        
        assert(
          IvyUtils.generateIvyFile(b).readLines.mkString("\n") === 
          """|<ivy-module version="1.0" xmlns:e="http://ant.apache.org/ivy/extra">
             |  <info organisation="MakerTestGroupID" module="b" revision="${maker.module.version}"/>
             |  <configurations>
             |    <conf name="default" transitive="false"/>
             |    <conf name="compile" transitive="false"/>
             |    <conf name="test" transitive="false"/>
             |  </configurations>
             |  <publications>
             |    <artifact name="b" type="pom"/>
             |    <artifact name="b" type="jar" ext="jar" conf="default"/>
             |  </publications>
             |  <dependencies defaultconfmapping="${ivy.default.conf.mappings}">
             |    <exclude org="MakerTestGroupID" module="a"/>
             |    <exclude org="MakerTestGroupID" module="b"/>
             |  </dependencies>
             |</ivy-module>""".stripMargin
        )

        assert(
          IvyUtils.generateIvyFile(c).readLines.mkString("\n") === 
          """|<ivy-module version="1.0" xmlns:e="http://ant.apache.org/ivy/extra">
             |  <info organisation="MakerTestGroupID" module="c" revision="${maker.module.version}"/>
             |  <configurations>
             |    <conf name="default" transitive="false"/>
             |    <conf name="compile" transitive="false"/>
             |    <conf name="test" transitive="false"/>
             |  </configurations>
             |  <publications>
             |    <artifact name="c" type="pom"/>
             |    <artifact name="c" type="jar" ext="jar" conf="default"/>
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
