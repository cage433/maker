package maker

import maker.project._
import maker.utils.FileUtils._
import java.io.File
import org.apache.commons.lang3.StringUtils
import maker.task.TaskResult._
import ch.qos.logback.classic.Level
import ch.qos.logback.classic.Level._
import task.BuildResult
import maker.task.Task
import maker.utils.os.{Command, CommandOutputHandler}
import scala.collection.mutable.{Map => MMap}
import maker.task.compile._
import scala.util.Properties
import scala.collection.immutable.Nil
import scala.xml.NodeSeq

/**
 * Maker's own self-build definition,
 */
object Maker {

  val extraPomInfo : List[NodeSeq] = {
    val devNodes = List("Alex McGuire", "Louis Botterill", "Sam Halliday").map{name => <developer><name>{name}</name></developer>}
    List(
      <name>Maker</name>
      <description>A scala build tool</description>
      <developers>{devNodes}</developers>
      <licenses>
        <license>
          <name>BSD 2 clause</name>
        </license>
      </licenses>
      <url>https:github.com/cage433/maker</url> 
      <scm>
        <url>https://github.com/cage433/maker</url> 
      </scm>
    )
  }

  def module(rootBasename : String, name :Option[String] = None, upstreamProjects : List[Module] = Nil, upstreamTestProjects : List[Module] = Nil) = {
    val root = file(rootBasename).asAbsoluteFile
    new Module(
      root,
      file(".").asAbsoluteFile,
      name.getOrElse(rootBasename),
      immediateUpstreamModules = upstreamProjects,
      immediateUpstreamTestModules = upstreamTestProjects
    ) with ClassicLayout {
      override def extraProjectPomInfo = extraPomInfo
      override def organization = Some("com.github.cage433")
    }
  }

  lazy val testReporter = module("test-reporter", name = Some("maker-test-reporter"))
  lazy val maker_ = module("maker")

  // Used to disambiguate which maker is running in the repl.
  def pwd = println(Properties.userDir)

}
