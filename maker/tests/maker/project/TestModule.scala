package maker.project

import java.io.File
import maker.utils.FileUtils._
import maker.MakerProps
import java.util.concurrent.ConcurrentHashMap
import sbt.inc.Analysis
import maker.task.compile._
import maker.utils.RichString._

class TestModule(
  root : File, 
  name : String,
  upstreamProjects : List[Module] = Nil,
  upstreamTestProjects : List[Module] = Nil,
  overrideProps : Option[MakerProps] = None,
  analyses :ConcurrentHashMap[File, Analysis] = new ConcurrentHashMap[File, Analysis]()
) extends Module(
  root, 
  name,
  upstreamProjects, 
  upstreamTestProjects, 
  props = overrideProps.getOrElse(TestModule.makeTestProps(root)),
  analyses
){
  root.mkdirs
  override def unmanagedLibDirs = List(file("utils/lib_managed"), file("test-reporter/lib_managed"))
  override def constructorCodeAsString : String = {
    """val %s = new TestModule(file("%s"), "%s", %s, %s)""" % (name, root.getAbsolutePath, name, 
      upstreamProjects.mkString("List(", ", ", ")"),
      upstreamTestProjects.mkString("List(", ", ", ")")
    )
  }
  def writeSrc(relativeSrcPath : String, code : String, phase : CompilePhase = SourceCompilePhase) = {
    val dirs = phase match {
      case SourceCompilePhase => sourceDirs
      case TestCompilePhase => testSourceDirs
    }
    val dir = dirs.head // we know we only have one
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

object TestModule{
  def makeTestProps(root : File) : MakerProps = {
    lazy val props = MakerProps()
    MakerProps(
      "ShowCompilerOutput", "false", 
      "GroupId", "MakerTestGroupID",
      "MakerHome", props.MakerHome(),
      "ProjectScalaCompilerJar", props.ProjectScalaCompilerJar().getPath,
      "ProjectScalaLibraryJar", props.ProjectScalaLibraryJar().getPath,
      "SbtInterfaceJar", props.SbtInterfaceJar().getPath,
      "CompilerInterfaceSourcesJar", props.CompilerInterfaceSourcesJar().getPath,
      "TmuxMessaging", "false",
      "ResourceCacheDirectory", file(root, ".maker-resource-cache").makeDirs().getPath,
      "PublishLocalRootDir", file(root, ".maker-publish-local").makeDirs().getPath,
    "MakerTestReporterJar", props.MakerTestReporterJar().getPath,
    "RunningInMakerTest", "true"
    )
  }

}
