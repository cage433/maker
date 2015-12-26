package maker.project

import sbt.inc.Analysis
import maker.task.compile._
import java.util.concurrent.ConcurrentHashMap
import org.eclipse.aether.util.artifact.JavaScopes
import java.io.File
import maker.utils.FileUtils._
import maker.utils.os.Command
import scala.language.postfixOps

case class TestModuleBuilder(
  root : File, 
  name : String,
  immediateUpstreamModuleNames : Seq[String] = Nil,
  testDependencies : Seq[String] = Nil,
  reportBuildResult : Boolean = false,
  scalatestOutputParameters : String = "-oHL",
  extraCode : String = "",
  systemExitOnExecModeFailures : Boolean = true,
  extraTraits : Seq[String] = Nil,
  extraDependencies : Seq[RichDependency] = Nil
) 
  extends DependencyPimps
{

  import TestModuleBuilder.listString
  def withBuildResult = copy(reportBuildResult = true)
  def withTestExceptions = copy(scalatestOutputParameters = "-oFHL")
  def withExtraCode(code : String) = copy(extraCode = code)
  def withNoExecModeExit = copy(systemExitOnExecModeFailures = false)
  def withExtraDependencies(deps : Seq[RichDependency]) = copy(extraDependencies = deps)

  def listString(list : Seq[String]) : String = {
    s"${list.mkString("List(", ", ", ")")}"
  }

  def dependencies : Seq[RichDependency] = 
        ("org.scalatest" % "scalatest" %% "2.2.0" withScope(JavaScopes.TEST)) +:
        ("com.github.cage433" % "maker-test-reporter" %% "0.09" withScope(JavaScopes.TEST)) +:
        extraDependencies

  def constructorCodeAsString : String = {
    s"""|
        |val $name = new Module(
        | new java.io.File("${root.getAbsolutePath}"), 
        |   "$name",
        |   compileDependencies = ${listString(immediateUpstreamModuleNames)},
        |   testDependencies = ${listString(testDependencies)}
        |)  ${("maker.project.DependencyPimps" +: "ClassicLayout" +: extraTraits).mkString(" with ", " with ", "")}{
        |
        |   override def dependencies = ${listString(dependencies.map(_.toLongString))}
        |   override def reportBuildResult = ${reportBuildResult}
        |   override def scalatestOutputParameters = "${scalatestOutputParameters}"
        |   override def systemExitOnExecModeFailures = ${systemExitOnExecModeFailures}
        |   override def scalacOptions = List("-unchecked", "-deprecation", "-feature", "-Xfatal-warnings")
        |   override def updateIncludesSourceJars = false
        |   $extraCode
        |} 
        |""".stripMargin
  }

  def appendDefinitionToProjectFile(rootDir : File){
    val projectFile = file(rootDir, "Maker.scala")
    appendToFile(
      projectFile,
      constructorCodeAsString
    )
  }

  def writeSrc(relativeSrcPath : String, code : String) = {
    writeToFile(file(root, "src", relativeSrcPath), code.stripMargin)
  }

  def writeTest(relativeSrcPath : String, code : String) = {
    writeToFile(file(root, "tests", relativeSrcPath), code.stripMargin)
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

}

object TestModuleBuilder{
  def createMakerProjectFile(rootDir : File){
    rootDir.mkdirs
    val projectFile = file(rootDir, "Maker.scala")
    val text = 
        s"""|
            |import maker.project.{Module, DependencyPimps, ClassicLayout, Project}
            |import maker.utils.FileUtils._
            |import org.eclipse.aether.util.artifact.JavaScopes
            |import java.io.File
            |""".stripMargin
    writeToFile(projectFile, text)
    writeLogbackConfig(rootDir)
  }

  def listString(list : Seq[String]) : String = {
    s"${list.mkString("List(", ", ", ")")}"
  }

  def appendTopLevelProjectDefinition(
    rootDir : File, 
    name : String, 
    upstreams : Seq[String],
    extraCode : String = ""
  ){
    val projectFile = file(rootDir, "Maker.scala")
    val text = 
      s"""|
          | val $name = new Project(
          |               "$name", file("${rootDir.absPath}"),
          |               compileDependencies = ${listString(upstreams)}
          |             ){
          |               $extraCode
          |             }
          |""".stripMargin
    appendToFile(projectFile, text)
  }

  def writeLogbackConfig(rootDir : File, level : String = "ERROR"){
    val configFile = file(rootDir, "logback.xml")
    writeToFile(
      configFile,
      s"""
        |<configuration scan="true" scanPeriod="3 seconds">
        |        
        |  <appender name="CONSOLE" class="ch.qos.logback.core.ConsoleAppender">
        |    <encoder>
        |      <pattern>%d{HH:mm:ss.SSS} [%thread] %-5level - %msg%n</pattern>
        |      <immediateFlush>true</immediateFlush>
        |    </encoder>
        |    <filter class="ch.qos.logback.classic.filter.ThresholdFilter">
        |      <level>$level</level>
        |    </filter>
        |  </appender>
        |
        |  <root level="$level">
        |    <appender-ref ref="CONSOLE" />
        |  </root>
        |</configuration>""".stripMargin
    )
  }

  def makerExecuteCommand(dir : File, method : String) = {
    val makerScript = file("maker.py").getAbsolutePath
    import scala.concurrent.duration._
    val command = Command(
      "python",
      makerScript,
      "-E",
      method,
      "-z",
      "-l",
      file(dir, "logback.xml").getAbsolutePath,
      "-L",
      "40"
    ).
    withWorkingDirectory(dir).
    withExitValues(0, 1).
    withTimeout(1 minute)

    command
  }

}

