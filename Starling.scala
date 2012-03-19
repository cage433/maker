import maker.project.Project
import java.io.File
import maker.Props
import maker.utils.FileUtils._
import maker.utils.Log
import org.apache.log4j.Level._
import org.apache.commons.io.FileUtils._

val properties = Props(file("Maker.conf"))

def project(name : String) = new Project(
  name, 
  file(name),
  libDirs = List(file(name, "lib_managed"), file(name, "lib"), file(name, "maker-lib"), file(".maker/scala-lib")),
  resourceDirs = List(file(name, "resources"), file(name, "test-resources")),
  props = properties
)

val manager = project("manager")
val utils = project("utils") dependsOn manager
val osgirun = project("osgirun").copy(libDirs = List(new File("osgirun/lib_managed"), new File("osgirun/lib"), new File("osgirun/osgi_jars")))
val booter = project("booter")
val concurrent = project("concurrent") dependsOn utils
val quantity = project("quantity") dependsOn utils
val osgiManager = project("osgimanager") dependsOn utils
val singleClasspathManager = project("singleclasspathmanager") dependsOn osgiManager
val pivot = project("pivot") dependsOn quantity
val daterange = project("daterange") dependsOn utils
val pivotUtils = project("pivot.utils") dependsOn (daterange, pivot)
val titanReturnTypes = project("titan.return.types") dependsOn (daterange, quantity)
val maths = project("maths") dependsOn (daterange, quantity)
val starlingApi = project("starling.api") dependsOn titanReturnTypes
val props = project("props") dependsOn utils
val auth = project("auth") dependsOn props
val bouncyrmi = project("bouncyrmi") dependsOn auth
val loopyxl = project("loopyxl") dependsOn auth
val browserService = project("browser.service") dependsOn manager
val browser = project("browser") dependsOn browserService
val guiapi = project("gui.api") dependsOn (browserService, bouncyrmi, pivotUtils)
val fc2Facility = project("fc2.facility") dependsOn guiapi
val curves = project("curves") dependsOn (maths, guiapi)
val instrument = project("instrument") dependsOn (curves, titanReturnTypes)
val reportsFacility = project("reports.facility") dependsOn guiapi
val rabbitEventViewerApi = project("rabbit.event.viewer.api") dependsOn(pivot, guiapi)
val tradeFacility = project("trade.facility") dependsOn guiapi
val gui = project("gui") dependsOn (fc2Facility, tradeFacility, reportsFacility, browser, rabbitEventViewerApi, singleClasspathManager)
val starlingClient = project("starling.client") dependsOn (starlingApi, bouncyrmi)
val dbx = project("dbx") dependsOn instrument
val databases = project("databases") dependsOn (pivot, concurrent, starlingApi, dbx)
val titan = project("titan") dependsOn (starlingApi, databases)
val services = project("services").copy(resourceDirs = List(new File("services", "resources"), new File("services", "test-resources"))) dependsOn (curves, concurrent, loopyxl, titan, gui, titanReturnTypes)
val services = project("services") dependsOn (curves, concurrent, loopyxl, titan, gui, titanReturnTypes)
val rabbitEventViewerService = project("rabbit.event.viewer.service") dependsOn (rabbitEventViewerApi, databases, services)
val tradeImpl = project("trade.impl") dependsOn (services, tradeFacility)
val metals = project("metals").copy(resourceDirs = List(new File("metals", "resources"), new File("metals", "test-resources"))) dependsOn tradeImpl
val reportsImpl = project("reports.impl") dependsOn services
val webservice = project("webservice") dependsOn (props, starlingApi)
val startserver = project("startserver") dependsOn (reportsImpl, metals, starlingClient, webservice, rabbitEventViewerService)
val launcher = project("launcher") dependsOn (startserver, booter)


// build a standard titan component (module) webapp  definition
def projectT(name : String) = {
  val titanService = "../" + name + "/service"
  new Project(
    name, 
    file(titanService),
    sourceDirs = List(file(titanService, "src/main/scala")),
    tstDirs = List(file(titanService, "src/test/scala")),
    libDirs = List(file(titanService, "lib_managed"),
      file(titanService, "lib"),
      file(titanService, "../../.maker/lib"),
      file(titanService, ".maker/scala-lib")),
    managedLibDirName = "lib_managed",
    resourceDirs = List(file(titanService, "src/main/resources")),
    props = properties,
    ivySettingsFile = file(titanService, "../../.maker/ivy/maker-ivysettings.xml"),
    webAppDir = Some(file(titanService, "src/main/webapp"))
  )
}

val titanConfig = projectT("configuration")
val titanMurdoch = projectT("murdoch")
val titanTradeService = projectT("tradeservice")
val titanPermission = projectT("permission")
val titanReferenceData = projectT("referencedata")
val titanLogistics = projectT("logistics")

val titanComponents = Seq(
//                titanConfig,
                titanMurdoch,
                titanTradeService,
//                titanPermission,
                titanReferenceData,
                titanLogistics)

val starlingTitanDeps = project("titanComponents") dependsOn (titanComponents : _*)

val titanLauncher = project("titan.launcher").dependsOn(launcher, starlingTitanDeps)

import maker.task.BuildResult
def buildWithTitan = {
  // should do below, but starling maker build needs fixing
  //val r = titanLauncher.compile
  val r : BuildResult = starlingTitanDeps.pack
  r.res match {
    case Right(_) => Some(r)
    case _ => None
  }
}
val jbossDeployDir = file(System.getenv("JBOSS_HOME") + "/server/trafigura/deploy")
val jettyDeployDir = file("titan.deploy/wars")
def deployWar(project : Project, deployDir : File) {
  copyFileToDirectory(file(project.root, "package/" + project.name + ".war"), deployDir)
}
def deployWarToJetty(project: Project) = deployWar(project, jettyDeployDir)
def deployWarsTo(deployDir : File) = {
  Log.info("Titan deploy to: " + deployDir.getAbsolutePath)
  titanComponents.foreach(p => deployWar(p, deployDir))
}
def deployToTitanJboss = deployWarsTo(jbossDeployDir)
def deployToTitanJetty = deployWarsTo(jettyDeployDir)

// build all of startling and titan dependencies for starling
// compile and package the titan web apps
// deploy them to local jboss
def buildAndDeployWithTitanJboss = buildWithTitan.map(_ => deployToTitanJboss)
def buildAndDeployWithTitanJetty = buildWithTitan.map(_ => deployToTitanJetty)

def runStarlingWithTitan = {
  titanLauncher.compile
  deployToTitanJetty
  titanLauncher.runMain(
    "starling.launcher.DevLauncher",
    "-server",
    "-XX:MaxPermSize=512m",
    "-Xss128k",
    "-Xms6000m",
    "-Xmx6000m",
    "-Dsun.awt.disablegrab=true",
    "-XX:UseConcMarkSweepGC",
    "-verbose:gc",
    "-XX:+PrintGCTimeStamps",
    "-XX:+PrintGCDetails",
    "-Djavax.xml.parsers.DocumentBuilderFactory=com.sun.org.apache.xerces.internal.jaxp.DocumentBuilderFactoryImpl",
    "-Dtitan.webapp.server.logs=logs/")
}

import java.io._

def writeToFile(fileName : String, text : String){
  val fstream = new FileWriter(fileName)
  val out = new BufferedWriter(fstream)
  out.write(text)
  out.close()
}

def writeClasspath{
  val cp = launcher.compilationClasspath
  writeToFile("launcher-classpath.sh", "export STARLING_CLASSPATH=" + cp)
}

