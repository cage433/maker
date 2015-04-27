package maker.project

import java.io.File
import maker.utils.FileUtils._
import maker._
import java.util.concurrent.ConcurrentHashMap
import sbt.inc.Analysis
import maker.task.compile._
import com.typesafe.config.{ConfigFactory, Config}
import org.eclipse.aether.util.artifact.JavaScopes
import org.apache.commons.io.{FileUtils => ApacheFileUtils}

class TestModule(
  root : File, 
  name : String,
  upstreamProjects : List[Module] = Nil,
  upstreamTestProjects : List[Module] = Nil,
  analyses :ConcurrentHashMap[File, Analysis] = new ConcurrentHashMap[File, Analysis](),
  config : Config = ConfigFactory.load()
) extends Module(
  root, 
  name,
  config,
  upstreamProjects, 
  upstreamTestProjects,
  analyses
) with ClassicLayout with DependencyPimps {

  override def constructorCodeAsString : String = {
    s"""|
        |val $name = new TestModule(
        | new java.io.File("${root.getAbsolutePath}"), 
        |   "$name",
        |   upstreamProjects = ${upstreamProjects.mkString("List(", ", ", ")")},
        |   upstreamTestProjects = ${upstreamTestProjects.mkString("List(", ", ", ")")}
        |)  with maker.project.DependencyPimps  {
        |   override def dependencies = List("org.scalatest" % "scalatest_2.10" % "2.2.0" withScope(JavaScopes.TEST))
        |}""".stripMargin
  }

  def appendDefinitionToProjectFile(rootDir : File){
    val projectFile = file(rootDir, "Maker.scala")
    appendToFile(
      projectFile,
      constructorCodeAsString
    )
  }
  override def dependencies = List(
    "org.scalatest" % "scalatest_2.10" % "2.2.0" withScope(JavaScopes.TEST),
    "com.github.cage433" % "maker-test-reporter" % "0.06" withScope(JavaScopes.TEST)
  )

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
  //writeMakerProjectDefinitionFile

  override def isTestProject = true

  def writeMakerProjectDefinitionFile{
    import maker.utils.RichString._
    val makerFile = file(rootAbsoluteFile, "Maker.scala")

    if (!makerFile.exists){
      val text = 
        s"""|
            | import maker.project.{TestModule, DependencyPimps}
            | import org.eclipse.aether.util.artifact.JavaScopes
            | import java.io.File
            |
            |val $name = new TestModule(
            | new File("${root.getAbsolutePath}"), 
            |   "$name",
            |   upstreamProjects = ${upstreamProjects.mkString("List(", ", ", ")")},
            |   upstreamTestProjects = ${upstreamTestProjects.mkString("List(", ", ", ")")}
            |)  with DependencyPimps  {
            |   override def dependencies = List("org.scalatest" % "scalatest_2.10" % "2.2.0" withScope(JavaScopes.TEST))
            |}
            |
            | import ${name}._
            |""".stripMargin
      writeToFile(makerFile, text)
    }
  }
}

object TestModule{
  def createMakerProjectFile(rootDir : File){
    rootDir.mkdirs
    val projectFile = file(rootDir, "Maker.scala")
    val text = 
        s"""|
            |import maker.project.{TestModule, DependencyPimps}
            |import org.eclipse.aether.util.artifact.JavaScopes
            |import java.io.File
            |""".stripMargin
    writeToFile(projectFile, text)
  }
}
trait HasDummyCompiler{
  self : TestModule => 
    override def compilerName = "dummy-test-compiler"
}
