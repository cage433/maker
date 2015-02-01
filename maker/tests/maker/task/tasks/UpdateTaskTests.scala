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


        val resolverDir = file(dir, "RESOLVER").makeDir

        val module = new TestModule(dir, "testResources"){
          override def resourceCacheDirectory = file(dir, ".maker", ".resource-cache").makeDirs
        }

        val externalResourceConfigFile = file(dir, "external-resource-config")
        writeToFile(
          externalResourceConfigFile,
          """|version: scala_version 2.9.2
             |version: sbt_version 0.12.1
             |version: scalatest_version 1.8
             |""".stripMargin
        )

        appendToFile(
          externalResourceConfigFile,
          ("""|resolver: default file://%s/RESOLVER/
              |resolver: second file://%s/RESOLVER2/""".stripMargin) % (dir.getAbsolutePath, dir.getAbsolutePath)
        )

        val expected = Set(
          Resource("org.foo", "bar", "{sbt_version}", downloadDirectory = Some(module.managedLibDir)).resolveVersions(module.resourceVersions()),
          Resource("com.mike", "fred_{scala_version}", "{scalatest_version}", 
            preferredRepository = Some("second"),
            downloadDirectory = Some(module.managedLibDir)
            ).resolveVersions(module.resourceVersions())
        )
        assert(module.resources().toSet === expected)

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

        // test jars not ignore the resource list are deleted when we update
        val oldJar = file(module.managedLibDir, "Foo.jar").touch
        assert(oldJar.exists, oldJar + " should exist")
        module.updateOnly
        assert(oldJar.doesNotExist, oldJar + " should not exist")
    }
  }

  "Update task should cause all upstream modules to update" in {
    withTempDir{
      dir => 
        val aDir = FileUtils.mkdir(file(dir, "a"))
        val bDir = FileUtils.mkdir(file(dir, "b"))
        writeToFile(
          file(aDir, "external-resources"),
          """|org.foo bar {sbt_version}
             |com.mike fred_{scala_version} {scalatest_version} resolver:second""".stripMargin
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
        val a = new TestModule(aDir, "a"){
          override def projectRoot = dir
        }
        val b = new TestModule(bDir, "b", upstreamProjects = List(a)){
          override def projectRoot = dir
        }

        val externalResourceConfigFile = file(dir, "external-resource-config")
        writeToFile(
          externalResourceConfigFile,
          """|version: scala_version 2.9.2
             |version: sbt_version 0.12.1
             |version: scalatest_version 1.8
             |""".stripMargin
        )

        appendToFile(
          externalResourceConfigFile, 
          ("""|resolver: default file://%s/RESOLVER/
             |resolver: second file://%s/RESOLVER2/""".stripMargin) % (dir.getAbsolutePath, dir.getAbsolutePath)
        )
        b.update
        a.resources.filter(_.isBinaryJarResource).foreach{
          resource => 
            assert(resource.resourceFile.exists, "Resource " + resource + " should exist")
        }
    }
  }

  "Updating source jars" in {
    withTempDir{
      dir =>

        writeToFile(
          file(dir, "external-resources"),
          """|org.foo bar 1.0
             |com.mike fred 2.0""".stripMargin
        )


        val resolverDir = file(dir, "RESOLVER").makeDir

        val module = new TestModule(dir, "testSourceJars")

        val externalResourceConfigFile = file(dir, "external-resource-config")
        writeToFile(
          externalResourceConfigFile,
          """|version: scala_version 2.9.2
             |version: sbt_version 0.12.1
             |version: scalatest_version 1.8
             |""".stripMargin
        )

        appendToFile(
          externalResourceConfigFile,
          ("""resolver: default file://%s/RESOLVER/""".stripMargin) % dir.getAbsolutePath
        )
        info("Update should also download any available source jars - nor failing when one is missing")

        writeToFile(
          file(dir, "/RESOLVER//com/mike/fred/2.0/fred-2.0.jar"),
          "foo"
        )
        writeToFile(
          file(dir, "/RESOLVER//com/mike/fred/2.0/fred-2.0-sources.jar"),
          "foo"
        )
        writeToFile(
          file(dir, "/RESOLVER//org/foo/bar/1.0/bar-1.0.jar"),
          "bar"
        )
        assert(module.updateOnly.succeeded, "Update should not fail if even if source jars are missing")

        assert(file(dir, "lib_managed/com.mike-fred-2.0.jar").exists)
        assert(file(dir, "lib_src_managed/com.mike-fred-2.0-sources.jar").exists)
        assert(file(dir, "lib_managed/org.foo-bar-1.0.jar").exists)

        info("updateSources should fail if source jars are missing")
        assert(module.updateSources.failed, "Update Source should have failed")

        info("Subsequent updates should not try to download source jars for which binaries exist")
        writeToFile(
          file(dir, "/RESOLVER//org/foo/bar/1.0/bar-1.0-sources.jar"),
          "bar"
        )
        assert(module.updateOnly.succeeded, "Update should not fail")
        assert(! file(dir, "lib_src_managed/org.foo-bar-1.0-sources.jar").exists)

 //       info("updateSources should try to download missing source jars")
//        assert(module.updateSources.succeeded, "Update Sources should not fail")
//        assert(file(dir, "lib_src_managed/org.foo-bar-1.0-sources.jar").exists)
    }
  }
}
