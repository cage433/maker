import _root_.maker.ProjectProps
import java.util.Properties
import java.io.File
import org.apache.log4j.Level._
import org.apache.commons.io.FileUtils._
import maker.project._
import maker.project.TopLevelProject
import maker.project.ProjectLib
import maker.Props
import maker.utils.FileUtils._
import maker.utils.Log
import maker.utils.Log._
import maker.RichProperties._
import maker.utils.os.Command
import maker.utils.os.Command._
import maker.utils.ModuleId._
import maker.utils.GroupAndArtifact
import maker.task.BuildResult

import Common._
import Utils._
import TitanModel._

object Starling {

  println("\n ** Loading (compiled) Starling build...\n")

  //repl.setPrompt("starling-maker>")

  lazy val makerProps : ProjectProps = file("Maker.conf")
  lazy val starlingProperties : Properties = file("props.conf")

  val targetDirName = "target-maker"
  def defaultStarlingLayout(root : File) = ProjectLayout.maker(root, Some(file(root, targetDirName)))

  def project(name : String) : Project = {
    val root = file(name)
    new Project(
      root,
      name,
      layout = defaultStarlingLayout(root),
      props = makerProps
    )
  }

  lazy val manager = project("manager")
  lazy val utils = project("utils") dependsOn manager
  lazy val osgirun = { val p = project("osgirun"); p.copy(layout = p.layout.withLibDirs(file("osgirun/lib"), file("osgirun/osgi_jars"))) }
  lazy val starlingDTOApi = project("starling.dto.api") dependsOn(utils :: trademgmtModelDeps : _*)
  lazy val booter = project("booter")
  lazy val quantity = project("quantity") dependsOn(starlingDTOApi)
  lazy val osgiManager = project("osgimanager") dependsOn utils
  lazy val singleClasspathManager = project("singleclasspathmanager") dependsOn osgiManager
  lazy val pivot = project("pivot") dependsOn quantity
  lazy val daterange = project("daterange") dependsOn(starlingDTOApi)
  lazy val pivotUtils = project("pivot.utils") dependsOn(daterange, pivot)
  lazy val maths = project("maths") dependsOn (daterange, quantity)
  lazy val props = project("props") dependsOn utils
  lazy val auth = project("auth") dependsOn props
  lazy val bouncyrmi = project("bouncyrmi") dependsOn auth
  lazy val loopyxl = project("loopyxl") dependsOn auth
  lazy val browserService = project("browser.service") dependsOn manager
  lazy val browser = project("browser") dependsOn browserService
  lazy val guiapi = project("gui.api") dependsOn (browserService, bouncyrmi, pivotUtils)
  lazy val fc2Facility = project("fc2.facility") dependsOn guiapi
  lazy val curves = project("curves") dependsOn (maths, guiapi)
  lazy val instrument = project("instrument") dependsOn curves
  lazy val reportsFacility = project("reports.facility") dependsOn guiapi
  lazy val rabbitEventViewerApi = project("rabbit.event.viewer.api") dependsOn guiapi
  lazy val tradeFacility = project("trade.facility") dependsOn guiapi
  lazy val gui = project("gui") dependsOn (fc2Facility, tradeFacility, reportsFacility, browser, rabbitEventViewerApi, singleClasspathManager)
  lazy val starlingClient = project("starling.client") /* withModuleId("starling-client" % "starling-client_2.9.1") */ dependsOn (starlingDTOApi, bouncyrmi)
  lazy val dbx = project("dbx") dependsOn (props)
  lazy val databases = project("databases") dependsOn (dbx, instrument)
  lazy val schemaevolution = project("schemaevolution") dependsOn (databases)
  lazy val titan = project("titan") dependsOn (auth, databases)
  lazy val services = project("services") dependsOn (loopyxl, titan, schemaevolution, fc2Facility, reportsFacility)
  lazy val rabbitEventViewerService = project("rabbit.event.viewer.service") dependsOn (rabbitEventViewerApi, services)
  lazy val tradeImpl = project("trade.impl") dependsOn (services, tradeFacility)
  lazy val oil = project("oil") dependsOn services
  lazy val metals = project("metals") dependsOn tradeImpl
  lazy val pnlreconcile = project("pnlreconcile") dependsOn (services, tradeFacility)
  lazy val reportsImpl = project("reports.impl") dependsOn (pnlreconcile)

  val hostTitanComponents = false
  val titanEnvAppServerLibs : List[File] = if (hostTitanComponents) file("webservice", "lib-jboss") :: Nil else Nil

  lazy val webservice = {
    val name = "webservice"
    val root = file(name)
    val libs = file(".maker/scala-lib") :: List("lib_managed", "lib", "maker-lib").map(file(root, _)) ::: titanEnvAppServerLibs
    val additionalModuleDeps = if (hostTitanComponents) logisticsModelDeps ::: trademgmtModelDeps else Nil
    val newLayout = defaultStarlingLayout(root).withLibDirs(libs : _*)
    new Project(
      root,
      name,
      layout = newLayout,
      props = makerProps
    ) dependsOn (instrument :: additionalModuleDeps : _*)
  }

  // below are some utils for running starling from maker
  lazy val startserver = project("startserver") dependsOn (reportsImpl, metals, oil, starlingClient, webservice, rabbitEventViewerService, singleClasspathManager)
  lazy val launcher = project("launcher") dependsOn (startserver, booter, gui)
  lazy val starling = new TopLevelProject("starling", List(launcher), makerProps,
    List(
      "logs",
      "osgi-gui-cache",
      "osgi-server-cache",
      "modulejarcache",
      ".maker",
      "target",
      "test-output",
      "test-suites",
      "maker/dist"
    ))

  def stdRunner(proj : Project)(className : String) = {
    proj.compile
    proj.runMain(className)(commonLaunchArgs : _*)()
  }
  val launcherRunner = stdRunner(launcher) _
  def runLauncher = launcherRunner("starling.launcher.Launcher")
  def runDevLauncher = launcherRunner("starling.launcher.DevLauncher")
  def runServer = launcherRunner("starling.startserver.Server")

  def writeStarlingClasspath() {
    val cp = launcher.classpathDirectoriesAndJars(SourceCompile).filterNot{file ⇒ file.getName.contains("scala-library") || file.getName.contains("scala-compiler")}
    val classpathString = "lib/scala/lib_managed/scala-library-jar-2.9.1.jar:" + cp.map(_.relativeTo(file("."))).map(_.getPath).filterNot(_.endsWith("-sources.jar")).toList.sortWith(_<_).mkString(":")
    writeToFile(file("bin/deploy-classpath.sh"), "export CLASSPATH=" + classpathString)
  }
}
