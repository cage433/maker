package maker.project

import org.scalatest.FunSuite
import maker.utils.FileUtils._
import maker.utils.os.Command
import maker.MakerProps

class ProjectTests extends FunSuite {
  test("Project can write its own definition file"){
    withTestDir{
      dir =>{
        val a = new TestModule(
          mkdir(file(dir, "a")),
          "a"
        )
        a.writeSrc(
          "foo/Foo.scala",
          """
          package foo

          case class Foo(a : Int)
          """
        )

        val b = new TestModule(
          mkdir(file(dir, "b")),
          "b",
          List(a)
        )
        b.writeTest(
          "foo/FooTest.scala",
          """
          package foo
          import org.scalatest.FunSuite
          class FooTest extends FunSuite{
            test("test foo"){
              assert(Foo(3).a === 3)
            }
          }
          """
        )

        val proj = Project(
          "TestProject",
          dir,
          List(a, b)
        )
        proj.writeMakerProjectDefinitionFile
        val makerDotSh = file("bin/maker.sh").absPath
        file("resource-resolvers").copyTo(dir)
        file("resource-versions").copyTo(dir)
        writeToFile(
          a.resourcesFile,
          "org.scalatest scalatest_{scala_version} {scalatest_version}"
        )

        val cmd = Command(
          MakerProps(file(dir, "props.conf")),
          makerDotSh,
          "-z",      // Developer mode prevents maker bootstrapping 
          "-e", "TestProject.test"
        ).withWorkingDirectory(dir)
        val result = cmd.exec
        assert(result === 0, "Expected project test to succeeed")
      }
    }
  }
}
