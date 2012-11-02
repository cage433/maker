import java.util.Properties
import java.io.File
import org.apache.commons.io.FileUtils._
import maker.project._
import maker.project.extras.TmuxMessaging
import maker.project.TopLevelProject
import maker.utils.FileUtils._
import maker.utils.os.Command
import maker.utils.os.Command._
import maker.utils.ModuleId._
import maker.utils.GroupAndArtifact
import maker.task.BuildResult
import maker.MakerProps

import Common._
import Utils._

object Starling {

  println("\n ** Loading (compiled) Starling build...\n")

  //repl.setPrompt("starling-maker>")

  lazy val makerProps : MakerProps = MakerProps(file("Maker.conf"))
  lazy val starlingProperties : Map[String, String] = MakerProps.propsFileToMap(file("props.conf"))

  val targetDirName = "target-maker"
  def defaultStarlingLayout(root : File) = ProjectLayout.maker(root, Some(file(root, targetDirName)))

  def project(name : String, upstreamProjects : List[Project], upstreamTestProjects : List[Project]) : Project with MoreSugar = {
    val root = file(name)
    new Project(
      root,
      name,
      layout = defaultStarlingLayout(root),
      upstreamProjects = upstreamProjects,
      upstreamTestProjects = upstreamTestProjects,
      props = makerProps
    ) with TmuxMessaging with MoreSugar
  }

  trait MoreSugar{
    self : Project ⇒
      def tcc = testCompileContinuously
      def stfe {
        props.ShowFailingTestException := ! props.ShowFailingTestException()
      }
  }

  def project(name : String, upstreamProjects : Project*) : Project = project(name, upstreamProjects.toList, Nil)

  lazy val manager = project("manager")
  lazy val utils = project("utils",  manager)
  lazy val osgirun = { val p = project("osgirun"); p.copy(layout = p.layout.withLibDirs(file("osgirun/lib"), file("osgirun/osgi_jars"))) }
  lazy val starlingDTOApi = project("starling.dto.api", utils)
  lazy val booter = project("booter")
  lazy val quantity = project("quantity", List(starlingDTOApi), List(utils))
  lazy val osgiManager = project("osgimanager",  utils)
  lazy val singleClasspathManager = project("singleclasspathmanager",  osgiManager)
  lazy val pivot = project("pivot",  quantity)
  lazy val daterange = project("daterange", starlingDTOApi)
  lazy val pivotUtils = project("pivot.utils", daterange, pivot)
  lazy val maths = project("maths", List(daterange, quantity), List(utils, quantity, daterange))
  lazy val props = project("props",  utils)
  lazy val auth = project("auth",  utils)
  lazy val bouncyrmi = project("bouncyrmi", auth)
  lazy val loopyxl = project("loopyxl",  auth)
  lazy val browserService = project("browser.service",  manager)
  lazy val browser = project("browser",  browserService)
  lazy val guiapi = project("gui.api", browserService, bouncyrmi, pivotUtils)
  lazy val fc2Facility = project("fc2.facility",  guiapi)
  lazy val curves = project("curves", List(maths, guiapi), List(utils, quantity, daterange))
  lazy val instrument = project("instrument",  List(curves), List(utils, quantity, curves, daterange, maths))
  lazy val reportsFacility = project("reports.facility",  guiapi)
  lazy val rabbitEventViewerApi = project("rabbit.event.viewer.api",  guiapi)
  lazy val tradeFacility = project("trade.facility",  guiapi)
  lazy val gui = project("gui", fc2Facility, tradeFacility, reportsFacility, browser, rabbitEventViewerApi, singleClasspathManager)
  lazy val starlingClient = project("starling.client", starlingDTOApi, bouncyrmi)
  lazy val dbx = project("dbx", props)
  lazy val databases = project("databases", List(dbx, instrument), List(utils, curves, daterange))
  lazy val schemaevolution = project("schemaevolution", databases)
  lazy val titan = project("titan", List(databases), List(utils, quantity, curves, daterange))
  lazy val services = project("services", List(loopyxl, titan, schemaevolution, fc2Facility, reportsFacility), List(utils, quantity, curves, daterange, instrument))
  lazy val rabbitEventViewerService = project("rabbit.event.viewer.service", rabbitEventViewerApi, services)
  lazy val tradeImpl = project("trade.impl", services, tradeFacility)
  lazy val pnlreconcile = project("pnlreconcile", List(services, tradeFacility), List(utils, curves, daterange))
  lazy val reportsImpl = project("reports.impl", List(pnlreconcile), List(utils, quantity, curves, daterange))
  lazy val metals = project("metals",  List(tradeImpl, reportsImpl), List(utils, daterange, curves))
  lazy val oil = project("oil",  services, reportsImpl)

  val hostTitanComponents = false
  val titanEnvAppServerLibs : List[File] = if (hostTitanComponents) file("webservice", "lib-jboss") :: Nil else Nil

  lazy val webservice = {
    val name = "webservice"
    val root = file(name)
    val libs = file(".maker/scala-lib") :: List("lib_managed", "lib", "maker-lib").map(file(root, _)) ::: titanEnvAppServerLibs
    val additionalModuleDeps = Nil
    val newLayout = defaultStarlingLayout(root).withLibDirs(libs : _*)
    new Project(
      root,
      name,
      layout = newLayout,
      props = makerProps,  
      upstreamProjects = (services :: additionalModuleDeps.toList),
      upstreamTestProjects = List(utils)
    )
  }

  // below are some utils for running starling from maker
  lazy val startserver = project("startserver", reportsImpl, metals, oil, starlingClient, webservice, rabbitEventViewerService, singleClasspathManager)
  lazy val launcher = project("launcher", List(startserver, booter, gui), List(curves))
  lazy val starling = new TopLevelProject("starling", List(launcher), makerProps,
    List(
      "logs",
      "osgi-gui-cache",
      "osgi-server-cache",
      "modulejarcache",
      ".maker",
      "target",
      "test-output",
      "maker/dist",
      "component-tests",
      "refined-service",
      "refinedtestclient"
    )) with TmuxMessaging with MoreSugar

  def stdRunner(proj : Project)(className : String) = {
    proj.compile
    proj.runMain(className)(commonLaunchArgs : _*)()
  }
  val launcherRunner = stdRunner(launcher) _
  def runLauncher = launcherRunner("starling.launcher.Launcher")
  def runDevLauncher = launcherRunner("starling.launcher.DevLauncher")
  def runServer = launcherRunner("starling.startserver.Server")

  def writeStarlingClasspath() {
    val cp = launcher.classpathDirectoriesAndJars(SourceCompilePhase).filterNot{file ⇒ file.getName.contains("scala-library") || file.getName.contains("scala-compiler")}
    val classpathString = "lib/scala/lib_managed/scala-library-jar-2.9.1.jar:" + cp.map(_.relativeTo(file("."))).map(_.getPath).filterNot(_.endsWith("-sources.jar")).toList.sortWith(_<_).mkString(":")
    writeToFile(file("bin/deploy-classpath.sh"), "export CLASSPATH=" + classpathString)
  }
}
