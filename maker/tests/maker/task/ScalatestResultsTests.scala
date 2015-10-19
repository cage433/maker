package maker.task

import org.scalatest.FunSuite
import scala.xml.MetaData
import scala.xml.Attribute
import maker.project.TestModule
import maker.utils.FileUtils._


class ScalatestResultsTests extends FunSuite{
  def metadataToMap(md : MetaData, acc : Map[String, String] = Map[String, String]()) : Map[String, String] = {
    md match {
      case scala.xml.Null => acc
      case a : Attribute => metadataToMap(md.next, acc ++ Map(a.key -> a.value.toString))
    }
  }

  test("Errors are correctly counted"){
    withTempDir{
      dir =>
        val proj = new TestModule(dir, "ScalatestResultsTests")
        proj.writeTest(
          "foo/FooTest.scala",
          """
          package foo
          import org.scalatest.FunSuite
          class FooTest extends FunSuite{
            test("test 1 == 1"){
              assert(1 === 1)
            }
            test("test 5 == 5"){
              assert(1 === 1)
            }
            test("test 1 == 2"){
              assert(1 === 2)
            }
          }
          """
        )

        proj.writeMakerProjectDefinitionFile
        proj.test
        assert(proj.testResults.passedTests.size === 2)
        assert(proj.testResults.failedTests.size === 1)
      }
  }
}
