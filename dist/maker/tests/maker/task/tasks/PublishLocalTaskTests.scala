package maker.task.tasks

import org.scalatest.FreeSpec
import maker.utils.FileUtils._
import maker.project.TestProject

class PublishLocalTaskTests extends FreeSpec{
  
  "Simple project should publish as expected" in {
    withTempDir{
      dir ⇒ 

        val proj = new TestProject(
          dir,
          "testPublishLocal"
        )

        proj.writeSrc(
          "testPublishLocal/Foo.scala",
          """
          |package testPublishLocal
          | 
          |case object Foo
          """.stripMargin
        )

        writeToFile(
          file(dir, "ivy.xml"),
          """
          |<ivy-module version="1.0" xmlns:e="http://ant.apache.org/ivy/extra">
          |  <info organisation="${group_id}" module="utils" revision="${maker.module.version}" />
          |  <configurations>
          |    <conf name="default" transitive="false"/>
          |    <conf name="compile" transitive="false"/>
          |    <conf name="test" transitive="false"/>
          |  </configurations>
          |
          |  <publications>
          |    <artifact name="utils" type="pom"/>
          |    <artifact name="utils" type="jar" ext="jar" conf="default" />
          |  </publications>
          |
          |  <dependencies defaultconfmapping="*->default">
          |    <dependency org="commons-io" name="commons-io" rev="2.1"/>
          |  </dependencies>
          |</ivy-module>
          """.stripMargin
        )

        writeToFile(
          file(dir, "ivysettings.xml"),
          """
          |<ivysettings>
          |  <settings>
          |    <settings name="default" transitive="false"/>
          |  </settings>
          |  <property name="ivy.local.default.root" value="${ivy.default.ivy.user.dir}/maker-local" override="false"/>
          |</ivysettings>
          """.stripMargin
        )


        assert(proj.compile.succeeded)
        proj.compilePhase.classFiles.foreach(println)
        println(proj.publishLocal)

    }
    
  }
}
