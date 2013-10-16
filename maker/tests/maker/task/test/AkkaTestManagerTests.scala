package maker.task.test

import org.scalatest.FunSuite
import maker.utils.FileUtils._
import maker.Props
import maker.project.Module
import maker.project.TestModule
import maker.project.MakerTestReporter

class AkkaTestManagerTests extends FunSuite {
  test("Dummy"){
  }
  ignore("Can receive test events via akka"){
    withTempDir{
      dir => 
        val props = Props.initialiseTestProps(dir)
        val moduleName = "AkkaTestReporterTest"
        val testManager = new AkkaTestManager()
        println("Debug: " + (new java.util.Date()) + " AkkaTestManagerTests: test manager is on port " + testManager.port)
        println("Debug: " + (new java.util.Date()) + " AkkaTestManagerTests: test manager is at address " + testManager.address)
        writeToFile(
          file(dir, "external-resources"),
          """|org.scalatest scalatest_{scala_version_base} {scalatest_version}
             |org.testng testng 6.2.1
             |com.beust jcommander 1.12
             |org.beanshell bsh 2.0b4
             |com.google.inject guice 2.0
             |org.scalatest scalatest_{scala_version_base} {scalatest_version}
             |com.typesafe.akka akka-actor_{scala_version_base} {akka_version} 
             |com.typesafe.akka akka-remote_{scala_version_base} {akka_version} 
             |com.typesafe.akka akka-slf4j_{scala_version_base} {akka_version} 
             |com.twitter util-core_{scala_version_base} {twitter_version}
             |org.scala-lang scala-reflect {scala_version} 
             |com.typesafe config {typesafe_config_version}
             |com.google.protobuf protobuf-java {protobuf_version}
             |io.netty netty {netty_version}
             """.stripMargin
        )
        val module = new Module(
          dir, moduleName, 
          props
        ) with TestModule{
          override def makerTestReporter = new MakerTestReporter{
            def scalatestReporterClass = "maker.scalatest.AkkaTestReporter"
            def scalatestClasspah = file(props.root, "maker-scalatest-reporter.jar").absPath
            def systemProperties : List[String]  = List(
              "-Dmaker.test.manager.port=" + testManager.port,
              "-Dmaker.test.module=" + moduleName
            )
            def results() = TestResults.EMPTY
          }
        }

        module.writeTest(
          "foo/Test1.scala",
          """
            package foo

            import org.scalatest.FunSuite

            class Test1 extends FunSuite{
              test("a test"){
                assert(1 === 1)
              }
            }
          """
        )

      assert(module.testCompile.succeeded, "Should compile")
      assert(module.test.succeeded, "Tests should have passed")

      assert(module.testResults.failedTestSuites.size === 1, "Ran one suite")
      

    }
  }
    
}

