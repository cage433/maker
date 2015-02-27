package maker.project

import java.io.File
import maker.utils.FileUtils._
import maker._
import java.util.concurrent.ConcurrentHashMap
import sbt.inc.Analysis
import maker.task.compile._
import maker.utils.RichString._
import com.typesafe.config.{ConfigFactory, Config}

class TestModule(
  root : File, 
  name : String,
  upstreamProjects : List[Module] = Nil,
  upstreamTestProjects : List[Module] = Nil,
  analyses :ConcurrentHashMap[File, Analysis] = new ConcurrentHashMap[File, Analysis](),
  config : Config = ConfigFactory.load()
) extends Module(
  root, 
  root,
  name,
  config,
  upstreamProjects, 
  upstreamTestProjects,
  analyses
) with ClassicLayout {
  root.mkdirs
  writeToFile(
    file(projectRoot, "external-resource-config"),
    """|resolver: default http://repo1.maven.org/maven2/
       |version: scala_version 2.10.4
       |version: sbt_version 0.13.5
       |version: scalatest_version 2.2.0""".stripMargin)
  List("scala-library", "scala-compiler", "scala-reflect").foreach{
    name => 
      val resource = Resource("org.scala-lang",name, "2.10.4")
      new ResourceUpdater(resource, config, projectScalaLibsDir).update()
  }
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
  writeMakerProjectDefinitionFile

  override def isTestProject = true
}


trait HasDummyCompiler{
  self : TestModule => 
    override def compilerName = "dummy-test-compiler"
}
