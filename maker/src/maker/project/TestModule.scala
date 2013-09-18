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
      file(root, "logback.xml"),
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

