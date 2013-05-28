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
import scala.collection.immutable.TreeMap

import Common._
import Utils._

object Starling {

  println("\n ** Loading (compiled) Starling build...\n")

  lazy val makerProps : MakerProps = MakerProps(file("Maker.conf"))
  lazy val starlingProperties : Map[String, String] = MakerProps.propsFileToMap(file("props.conf"))

  val targetDirName = "target-maker"

  // the following libraries conflict with those in titan poms and break things, they are excluded from the
  // starling build as we provide our own definition of them
  val titanGlobalExclusions = "org.testng" % "testng" :: "org.slf4j" % "jcl-over-slf4j" :: Nil
  val globalDependencyAdjustments = DependencyAdjustments(Nil, titanGlobalExclusions, Nil)

  def project(name : String, upstreamProjects : List[Project], upstreamTestProjects : List[Project]) : Project with MoreSugar = {
    val root = file(name)
    new Project(
      root,
      name,
      layout = new MakerProjectLayout(root),
      upstreamProjects = upstreamProjects,
      upstreamTestProjects = upstreamTestProjects,
      props = makerProps,
      dependencyAdjustments = globalDependencyAdjustments
    ) with TmuxMessaging with MoreSugar
  }

  def reportRedundantJars(){
    val projects = starling.allUpstreamProjects
    val jarsByProject = projects.map{
      p ⇒ (p, p.classpathJarsOnly.map(_.getName).filterNot(_.contains("scala-library")).toSet)
    }.toMap
    projects.foreach{
      var redundancies = TreeMap[String, List[String]]()
      p ⇒ 
        jarsByProject(p).foreach{
          jarName ⇒ 
            p.allStrictlyUpstreamProjects.filter{
              u ⇒ 
                jarsByProject(u).contains(jarName)
            } match {
              case Nil ⇒ 
              case ps ⇒ redundancies += (jarName -> ps.map(_.name))
            }
        }
        if (redundancies.nonEmpty){
          val errorLines = redundancies.map{
            case (jarName, upstreams) => "\t " + jarName + " is contained in " + upstreams.mkString(", ")
          }
          println(errorLines.mkString("Project " + p + " has reduntant jars\n", "\n", ""))
        }
        redundancies = TreeMap[String, List[String]]()
    }
  }

  trait MoreSugar{
    self : Project ⇒
      def tcc = testCompileContinuously
      def stfe {
        props.ShowFailingTestException := ! props.ShowFailingTestException()
      }
  }

  def project(name : String, upstreamProjects : Project*) : Project = project(name, upstreamProjects.toList, Nil)

  // projects ordered by layer, then alphabetically, each layer depends only on layers above
  // the dependencies of each project are also ordered by their position in this list.
  // OCD yes but helps to visualise the project. There is a school of thought that too many layers is bad, we have 19!

  // Layer 1
  lazy val booter = project("booter")
  lazy val loopyxlJava = project("loopyxl-java")
  lazy val manager = project("manager")
  // Layer 2
  lazy val browserService = project("browser.service", manager)
  lazy val utils = project("utils", manager)
  // Layer 3
  lazy val browser = project("browser", browserService)
  lazy val props = project("props", utils)
  lazy val starlingDTOApi = project("starling.dto.api", utils)
  // Layer 4
  lazy val daterange = project("daterange", starlingDTOApi)
  lazy val quantity = project("quantity", List(starlingDTOApi), List(utils))
  lazy val singleClasspathManager = project("singleclasspathmanager", utils)
  // Layer 5
  lazy val auth = project("auth", daterange)
  lazy val dbx = project("dbx", props, daterange)
  lazy val maths = project("maths", List(daterange, quantity), List(daterange, quantity))
  lazy val pivot = project("pivot", List(quantity), List(utils))
  // Layer 6
  lazy val bouncyrmi = project("bouncyrmi", List(auth), List(utils))
  lazy val loopyxl = project("loopyxl", loopyxlJava, auth)
  lazy val pivotUtils = project("pivot.utils", daterange, pivot)
  lazy val schemaevolution = project("schemaevolution", dbx) // Please do not change this dependency without asking
  // Layer 7
  lazy val guiapi = project("gui.api", browserService, bouncyrmi, pivotUtils)
  lazy val starlingClient = project("starling.client", bouncyrmi)
  // Layer 8
  lazy val curves = project("curves", List(maths, guiapi), List(daterange, quantity))
  lazy val eventViewerApi = project("event.viewer.api", guiapi)
  lazy val fc2Facility = project("fc2.facility",  guiapi)
  lazy val reportsFacility = project("reports.facility", guiapi)
  lazy val tradeFacility = project("trade.facility", guiapi)
  // Layer 9
  lazy val gui = project("gui", browser, singleClasspathManager, eventViewerApi, fc2Facility, reportsFacility, tradeFacility)
  lazy val instrument = project("instrument", List(curves), List(maths, curves))
  // Layer 10
  lazy val databases = project("databases", List(dbx, instrument), List(curves))
  // Layer 11
  lazy val services = project("services", List(loopyxl, fc2Facility, reportsFacility, databases), List(instrument))
  // Layer 12
  lazy val tradeImpl = project("trade.impl", tradeFacility, services)
  lazy val pnlreconcile = project("pnlreconcile", List(tradeFacility, services), List(schemaevolution, curves))
  lazy val webservice = project("webservice", List(services), List(utils))
  // Layer 13
  lazy val eventstoreServer = project("eventstore-server", List(webservice), List(utils))
  lazy val reportsImpl = project("reports.impl", List(schemaevolution, pnlreconcile), List(databases))
  // Layer 14
  lazy val oil = project("oil", webservice, reportsImpl)
  lazy val titan = project("titan", List(starlingClient, webservice, reportsImpl), List(instrument, webservice))
  // Layer 15
  lazy val eventViewerService = project("event.viewer.service", eventViewerApi, titan)
  // Layer 16
  lazy val startserver = project("startserver", singleClasspathManager, tradeImpl, oil, webservice, eventViewerService)
  // Layer 17
  lazy val launcher = project("launcher", List(booter, gui, startserver), List(curves))
  // Layer 18
  lazy val starling = new TopLevelProject("starling", List(launcher, eventstoreServer), makerProps,
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

  reportRedundantJars()

  // below are some utils for running starling from maker
  def stdRunner(proj : Project)(className : String) = {
    proj.compile
    proj.runMain(className)(commonLaunchArgs : _*)()
  }
  val launcherRunner = stdRunner(launcher) _
  def runLauncher = launcherRunner("starling.launcher.Launcher")
  def runDevLauncher = launcherRunner("starling.launcher.DevLauncher")
  def runServer = launcherRunner("starling.startserver.Server")

  def writeStarlingClasspath() {
    val cp = launcher.compilePhase.classpathDirectoriesAndJars
    val classpathString = cp.map(_.relativeTo(file("."))).map(_.getPath).filterNot(_.endsWith("-sources.jar")).toList.sortWith(_<_).mkString(":")
    writeToFile(file("bin/deploy-classpath.sh"), "export CLASSPATH=" + classpathString)
  }

  def writeStarlingClasspathWithTests() {
    val cp = launcher.testCompilePhase.classpathDirectoriesAndJars
    val classpathString = cp.map(_.relativeTo(file("."))).map(_.getPath).filterNot(_.endsWith("-sources.jar")).toList.sortWith(_<_).mkString(":")
    writeToFile(file("bin/deploy-classpath.sh"), "export CLASSPATH=" + classpathString)
  }

  def publishLocalStarlingDeps(version : String) = {
    starlingClient.update
    quantity.update
    daterange.update
    starlingClient.Publish("local-m2-publish", version).execute
    quantity.Publish("local-m2-publish", version).execute
    daterange.Publish("local-m2-publish", version).execute
  }
}
