package maker.project

import org.scalatest.FunSuite
import maker.utils.FileUtils._
import maker.utils.os.Command
import maker.MakerProps


class TestModuleTests extends FunSuite{
  test("Can build test module from itself"){
    withTempDir{
      dir => 
        val proj = new TestModule(dir, "TestTestModule")
        proj.writeSrc(
          "foo/Foo.scala",
          """
          package foo
          case class Foo(a : Int)
          """
        )
        proj.writeTest(
          "foo/FooScalaTest.scala",
          """
          package foo
          import org.scalatest.FunSuite
          class FooScalaTest extends FunSuite{
            test("Test Foo"){
              val foo = Foo(3)
              assert(foo.a == 3)
            }
          }
          """
        )
        proj.writeMakerProjectDefinitionFile
        val makerDotSh = file("bin/maker.sh").absPath
        file("resource-resolvers").copyTo(dir)
        file("resource-versions").copyTo(dir)
        writeToFile(
          proj.resourcesFile,
          "org.scalatest scalatest_{scala_version} {scalatest_version}"
        )

        val cmd = Command(
          MakerProps(file(dir, "props.conf")),
          makerDotSh,
          "-z",      // Developer mode prevents maker bootstrapping 
          "-e", "TestTestModule.test"
        ).withWorkingDirectory(dir)
        val result = cmd.exec
        assert(result === 0, "Expected project test to succeeed")
    }
  }
}
