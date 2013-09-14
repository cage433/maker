package maker.task.tasks

import org.scalatest.FreeSpec
import maker.utils.FileUtils._
import maker.utils.RichString._
import maker.project.TestModule
import maker.Resource

class UpdateTaskTests extends FreeSpec {
  "test resources" in {
    withTempDir{
      dir => 
        writeToFile(
          file(dir, "external-resources"),
          """|org.foo bar {sbt_version}
             |com.mike fred_{scala_version} {scalatest_version} resolver:second""".stripMargin
        )
        val versionsFile = file(dir, "versions")
        writeToFile(
          versionsFile,
          """|scala_version 2.9.2
             |sbt_version 0.12.1
             |scalatest_version 1.8""".stripMargin
        )
        val resolversFile = file(dir, "resolvers")

        writeToFile(
          resolversFile, 
          ("""|default file://%s/RESOLVER/
             |second file://%s/RESOLVER2/""".stripMargin) % (dir.getAbsolutePath, dir.getAbsolutePath)
        )
        val resolverDir = file(dir, "RESOLVER").makeDir

        val props = TestModule.makeTestProps(dir) ++ (
          "VersionsFile", versionsFile.getAbsolutePath, 
          "ResolversFile", resolversFile.getAbsolutePath,
          "ResourceCacheDirectory", file(dir, ".maker-resource-cache").makeDirs().getPath
        )

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
}
