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

  def module(name : String, upstreamProjects : List[Module] = Nil, upstreamTestProjects : List[Module] = Nil) = {
    val root = file(name).asAbsoluteFile
    new Module(
      root,
      name,
      immediateUpstreamModules = upstreamProjects,
      immediateUpstreamTestModules = upstreamTestProjects,
      props = props
    ) with ClassicLayout
  }

  lazy val testReporter = module("test-reporter")
  lazy val utils = module("utils", List(testReporter))
  lazy val mkr = module("maker", List(utils), List(utils))

  lazy val topLevel = new Project("top-level", file("."), List(mkr), props)

  // Used to disambiguate which maker is running in the repl.
  def pwd = println(Properties.userDir)

}
