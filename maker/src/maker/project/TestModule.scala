package maker.project

import java.io.File
import maker.utils.FileUtils._
import maker.MakerProps
import java.util.concurrent.ConcurrentHashMap
import sbt.inc.Analysis
import maker.task.compile._
import maker.utils.RichString._
import maker.utils.RichIterable._
import maker.Resource
import maker.task.Task
import maker.task.TaskResult
import maker.utils.Stopwatch
import maker.task.tasks.UpdateTask
import maker.task.Build
import maker.task.Dependency
import org.apache.commons.io.{FileUtils => ApacheFileUtils}

class TestModule(
  root : File, 
  name : String,
  override val props : MakerProps,
  upstreamProjects : List[Module] = Nil,
  upstreamTestProjects : List[Module] = Nil,
  analyses :ConcurrentHashMap[File, Analysis] = new ConcurrentHashMap[File, Analysis]()
) extends Module(
  root, 
  name,
  props,
  upstreamProjects, 
  upstreamTestProjects, 
  analyses
){
  root.mkdirs
  override def unmanagedLibDirs = List(file("utils/lib_managed"), file("test-reporter/lib_managed"))
  override def constructorCodeAsString : String = {
    """val %s = new maker.project.Module(new java.io.File("%s"), "%s", maker.MakerProps(new java.io.File("%s")), %s, %s)""" % (name, root.getAbsolutePath, 
      name, 
      props.root.getAbsolutePath + "/Maker.conf",
      upstreamProjects.mkString("List(", ", ", ")"),
      upstreamTestProjects.mkString("List(", ", ", ")")
    )
  }
  def writeSrc(relativeSrcPath : String, code : String, phase : CompilePhase = SourceCompilePhase) = {
    val dir = phase match {
      case SourceCompilePhase => sourceDir
      case TestCompilePhase => testSourceDir
    }
    writeToFile(file(dir, relativeSrcPath), code.stripMargin)
  }
  def writeTest(relativeSrcPath : String, code : String) = writeSrc(relativeSrcPath, code, TestCompilePhase)

  val logFile = file(root, "maker.log")
  val patternLine = "<pattern>%d{HH:mm:ss.SSS} [%thread] %-5level - %msg%n</pattern>"
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
    """ % (logFile.getAbsolutePath, patternLine)).stripMargin
  )
  writeMakerProjectDefinitionFile
}
