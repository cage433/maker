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
import maker.utils.os.Command
import maker.utils.os.CommandOutputHandler
import scala.collection.mutable.{Map => MMap}
import maker.task.compile._
import scala.util.Properties

/**
 * Maker's own self-build definition,
 */
object Maker {

  private val props = MakerProps() ++ (
    "GroupId", "com.github.cage433",
    "LogbackTestConfigFile", "logback-config/logback-unit-tests.xml"
  )

  def module(rootBasename : String, name :Option[String] = None, upstreamProjects : List[Module] = Nil, upstreamTestProjects : List[Module] = Nil) = {
    val root = file(rootBasename).asAbsoluteFile
    new Module(
      root,
      name.getOrElse(rootBasename),
      immediateUpstreamModules = upstreamProjects,
      immediateUpstreamTestModules = upstreamTestProjects,
      props = props
    ) with ClassicLayout
  }

  lazy val testReporter = module("test-reporter", name = Some("maker-test-reporter"))
  lazy val utils = module("utils", upstreamProjects = List(testReporter))
  lazy val mkr = module("maker", upstreamProjects = List(utils), upstreamTestProjects = List(utils))

  lazy val topLevel = new Project("top-level", file("."), List(mkr), props)

  // Used to disambiguate which maker is running in the repl.
  def pwd = println(Properties.userDir)

}
