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
) with ClassicLayout {
  root.mkdirs
  override def unmanagedLibDirs = List(file("utils/lib_managed"), file("test-reporter/lib_managed"))
  override def constructorCodeAsString : String = {
    """val %s = new TestModule(file("%s"), "%s", %s, %s)""" % (name, root.getAbsolutePath, name, 
      upstreamProjects.mkString("List(", ", ", ")"),
      upstreamTestProjects.mkString("List(", ", ", ")")
    )
  }
  def writeSrc(relativeSrcPath : String, code : String, phase : CompilePhase = SourceCompilePhase) = {
    val dir = sourceDirs(phase).head // we know we only have one
    writeToFile(file(dir, relativeSrcPath), code.stripMargin)
  }

  /** A minimal piece of code to guarantee some compilation
    * is done and at least one class file produced
    */
  def writeCaseObject(objectName : String, packagePath : String*){
    val relativePath = packagePath.mkString("", "/", "/") + objectName + ".scala"
    val pckg = packagePath.mkString(".")
    writeSrc(relativePath,
    s"""
    |package $pckg
    |
    |case object $objectName
    """.stripMargin
    )

  }

  def writeTest(relativeSrcPath : String, code : String) = writeSrc(relativeSrcPath, code, TestCompilePhase)
  def addExternalResource(resourceString : String){
    appendToFile(file(root, "external-resources"), resourceString)
  }
  def addUnmanagedResource(path : String*){
    file(resourceDir(SourceCompilePhase), path : _*).touch
  }

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
      "TmuxMessaging", "false",
      "ResourceCacheDirectory", file(root, ".maker-resource-cache").makeDirs().getPath,
      "PublishLocalRootDir", file(root, ".maker-publish-local").makeDirs().getPath,
      "MakerTestReporterJar", props.MakerTestReporterJar().getPath,
      "RunningInMakerTest", "true"
    )
  }

}
