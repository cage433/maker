println("\n ** Loading Starling build...\n")


//repl.setPrompt("starling-maker>")

lazy val makerProps : Props = file("Maker.conf")
lazy val starlingProperties : Properties = file("props.conf")
starlingProperties.setProperty("log4j.configuration", "utils/resources/log4j.properties")

val targetDirName = "target-maker"
def defaultStarlingLayout(root : File) = ProjectLayout.maker(root, Some(file(root, targetDirName)))

def project(name : String) = {
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
lazy val schemaevolution = project("schemaevolution") dependsOn (dbx)
lazy val databases = project("databases") dependsOn (dbx, instrument)
lazy val titan = project("titan") dependsOn databases
lazy val services = project("services") dependsOn (loopyxl, titan, schemaevolution, fc2Facility, reportsFacility)
lazy val rabbitEventViewerService = project("rabbit.event.viewer.service") dependsOn (rabbitEventViewerApi, services)
lazy val tradeImpl = project("trade.impl") dependsOn (services, tradeFacility)
lazy val oil = project("oil") dependsOn services
lazy val metals = project("metals") dependsOn tradeImpl
lazy val reportsImpl = project("reports.impl") dependsOn services

val hostTitanComponents = true
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
lazy val starling = new TopLevelProject("starling", List(launcher), makerProps, List(ProjectLib(manager.name, true)),
  List(
    "logs",
    "osgi-gui-cache",
    "osgi-server-cache",
    "modulejarcache",
    ".maker",
    "target",
    "test-output",
    "test-suites"
  ))

def stdRunner(proj : Project)(className : String) = {
  proj.compile
  proj.runMain(className)(commonLaunchArgs : _*)()
}
val launcherRunner = stdRunner(launcher) _
def runLauncher = launcherRunner("starling.launcher.Launcher")
def runDevLauncher = launcherRunner("starling.launcher.DevLauncher")
def runServer = launcherRunner("starling.startserver.Server")

def writeStarlingClasspath {
  val cp = launcher.compilationClasspath(SourceCompile)
  writeToFile(file("launcher-classpath.sh"), "export STARLING_CLASSPATH=" + cp)
}
