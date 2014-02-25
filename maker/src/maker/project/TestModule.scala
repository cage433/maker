package maker.project

import java.io.File
import maker.utils.FileUtils._
import maker.Props
import java.util.concurrent.ConcurrentHashMap
import sbt.inc.Analysis
import maker.task.compile._
import maker.utils.Implicits.RichString._
import maker.utils.Implicits.RichIterable._
import maker.task.update.Resource
import maker.task.Task
import maker.task.TaskResult
import maker.utils.Stopwatch
import maker.task.update.UpdateTask
import maker.build.Build
import maker.build.Dependency
import org.apache.commons.io.{FileUtils => ApacheFileUtils}
import maker.task.test.AkkaTestManager

/**
  * Mixin with helper methods for maker unit tests
  */
trait TestModule{
  self : Module => 

    override val analyses = new ConcurrentHashMap[File, Analysis]()
    def writeSrc(relativeSrcPath : String, code : String, phase : CompilePhase = SourceCompilePhase) = {
      val dir = phase match {
        case SourceCompilePhase => self.sourceDir
        case TestCompilePhase => self.testSourceDir
      }
      writeToFile(file(dir, relativeSrcPath), code.stripMargin)
    }
    def writeTest(relativeSrcPath : String, code : String) = writeSrc(relativeSrcPath, code, TestCompilePhase)

    writeToFile(
      file(props.root, "logback.xml"),
      ("""
        |<configuration>
        |  <appender name="FILE" class="ch.qos.logback.core.FileAppender">
        |    <file>%s</file>
        |    <append>true</append>
        |    <encoder>
        |      %s
        |      <immediateFlush>true</immediateFlush>
        |    </encoder>
        |  </appender>
        |        
        |
        |  <root level="info">
        |    <appender-ref ref="FILE" />
        |  </root>
        |</configuration>
      """ % (file(root, "maker.log").getAbsolutePath, "<pattern>%d{HH:mm:ss.SSS} [%thread] %-5level - %msg%n</pattern>")).stripMargin
    )
    writeToFile(
      file(root, "external-resources"),
      """|org.scalatest scalatest_{scala_version} {scalatest_version}
          |com.typesafe.akka akka-actor {akka_version} 
          |com.typesafe.akka akka-remote {akka_version} 
          |com.typesafe.akka akka-slf4j {akka_version} 
          |com.twitter util-core_{scala_version} {twitter_version}
          |com.typesafe config {typesafe_config_version}
          |com.google.protobuf protobuf-java {protobuf_version}
          |io.netty netty {netty_version}
          |org.slf4j slf4j-api 1.6.1
          |ch.qos.logback logback-classic 1.0.6
          |ch.qos.logback logback-core 1.0.6
          """.stripMargin
    )
    file("maker-resource-config").copyTo(root)
    file("maker-scalatest-reporter.jar").copyTo(root)
    self.writeMakerProjectDefinitionFile

}

object TestModule{
  def apply(
    root : File, 
    name : String,
    props : Props,
    upstreamProjects : List[Module] = Nil,
    upstreamTestProjects : List[Module] = Nil
  ) : Module with TestModule = {
    new Module(root, name, props, upstreamProjects, upstreamTestProjects) with TestModule
  }
}

