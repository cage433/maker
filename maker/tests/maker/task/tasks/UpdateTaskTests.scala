package maker.task.tasks

import org.scalatest.FreeSpec
import maker.utils.FileUtils._
import maker.utils.RichString._
import maker.project.TestModule
import maker.Resource
import maker.utils.FileUtils

class UpdateTaskTests extends FreeSpec {
  "Update task should download resources" in {
    withTempDir{
      dir => 
        writeToFile(
          file(dir, "external-resources"),
          """|org.foo bar {sbt_version}
             |com.mike fred_{scala_version} {scalatest_version} resolver:second""".stripMargin
        )

        val externalResourceConfigFile = file(dir, "external-resource-config")
        writeToFile(
          externalResourceConfigFile,
          """|v: scala_version 2.9.2
             |v: sbt_version 0.12.1
             |v: scalatest_version 1.8
             |""".stripMargin
        )

        appendToFile(
          externalResourceConfigFile,
          ("""|r: default file://%s/RESOLVER/
              |r: second file://%s/RESOLVER2/""".stripMargin) % (dir.getAbsolutePath, dir.getAbsolutePath)
        )

        val resolverDir = file(dir, "RESOLVER").makeDir

        val props = TestModule.makeTestProps(dir) ++ ("ExternalResourceConfigFile", externalResourceConfigFile.getAbsolutePath)

        val module = new TestModule(dir, "testResources", overrideProps = Some(props))

        assert(
          module.resources().toSet === Set(
            Resource(module, "org.foo", "bar", "0.12.1"),
            Resource(module, "org.foo", "bar", "0.12.1", classifier=Some("sources")),
            Resource(module, "com.mike", "fred_2.9.2", "1.8", "jar", preferredRepository = Some("file://%s/RESOLVER2/" % dir.getAbsolutePath)),
            Resource(module, "com.mike", "fred_2.9.2", "1.8", "jar", preferredRepository = Some("file://%s/RESOLVER2/" % dir.getAbsolutePath), classifier=Some("sources"))
          )
        )

        assert(module.updateOnly.failed, "Update should fail before resources are available")
        writeToFile(
          file(dir, "/RESOLVER2//com/mike/fred_2.9.2/1.8/fred_2.9.2-1.8.jar"),
          "foo"
        )
        writeToFile(
          file(dir, "/RESOLVER//org/foo/bar/0.12.1/bar-0.12.1.jar"),
          "bar"
        )
        assert(module.updateOnly.succeeded, "Update should fail before resources are available")

        // test jars not in the resource list are deleted when we update
        val oldJar = file(module.managedLibDir, "Foo.jar").touch
        assert(oldJar.exists, oldJar + " should exist")
        module.updateOnly
        assert(oldJar.doesNotExist, oldJar + " should not exist")
    }
  }

  "Update task should cause all upstream modules to update" in {
    withTestDir{
      dir => 
        val aDir = FileUtils.mkdir(file(dir, "a"))
        val bDir = FileUtils.mkdir(file(dir, "b"))
        writeToFile(
          file(aDir, "external-resources"),
          """|org.foo bar {sbt_version}
             |com.mike fred_{scala_version} {scalatest_version} resolver:second""".stripMargin
        )
        val externalResourceConfigFile = file(dir, "external-resource-config")
        writeToFile(
          externalResourceConfigFile,
          """|v: scala_version 2.9.2
             |v: sbt_version 0.12.1
             |v: scalatest_version 1.8
             |""".stripMargin
        )

        appendToFile(
          externalResourceConfigFile, 
          ("""|r: default file://%s/RESOLVER/
             |r: second file://%s/RESOLVER2/""".stripMargin) % (dir.getAbsolutePath, dir.getAbsolutePath)
        )
        val resolverDir = file(dir, "RESOLVER").makeDir

        writeToFile(
          file(dir, "/RESOLVER2//com/mike/fred_2.9.2/1.8/fred_2.9.2-1.8.jar"),
          "foo"
        )
        writeToFile(
          file(dir, "/RESOLVER//org/foo/bar/0.12.1/bar-0.12.1.jar"),
          "bar"
        )
        val props = TestModule.makeTestProps(aDir) ++ ("ExternalResourceConfigFile", externalResourceConfigFile.getAbsolutePath)

        val a = new TestModule(aDir, "a", overrideProps = Some(props))
        val b = new TestModule(bDir, "b", overrideProps = Some(props), upstreamProjects = List(a))

        b.update
        a.resources.filter(_.isBinaryJarResource).foreach{
          resource => 
            assert(resource.resourceFile.exists, "Resource " + resource + " should exist")
        }
    }
  }
}
