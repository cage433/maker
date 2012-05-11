println("\n ** Loading Starling build...\n")


//repl.setPrompt("starling-maker>")

lazy val makerProps : Props = file("Maker.conf")
lazy val starlingProperties : Properties = file("props.conf")

def project(name : String) = {
  val root = file(name)
  new Project(
    name, 
    root,
    sourceDirs = file(root, "src") :: Nil,
    tstDirs = file(root, "tests") :: Nil,
    libDirs = List("lib_managed", "lib", "maker-lib", "scala-lib").map(file(name, _)),
    resDirs = List("resources", "test-resources").map(file(name, _)),
    props = makerProps,
    unmanagedProperties = starlingProperties
  )
}

lazy val manager = project("manager")
lazy val utils = project("utils") dependsOn manager
lazy val osgirun = project("osgirun").copy(libDirs = List(file("osgirun/lib_managed"), file("osgirun/lib"), file("osgirun/osgi_jars")))
lazy val booter = project("booter")
lazy val concurrent = project("concurrent") dependsOn utils
lazy val titanReturnTypes = project("titan.return.types")
lazy val quantity = project("quantity") dependsOn(utils, titanReturnTypes)
lazy val osgiManager = project("osgimanager") dependsOn utils
lazy val singleClasspathManager = project("singleclasspathmanager") dependsOn osgiManager
lazy val pivot = project("pivot") dependsOn quantity
lazy val daterange = project("daterange") dependsOn(utils, titanReturnTypes)
lazy val pivotUtils = project("pivot.utils") dependsOn(daterange, pivot)
lazy val starlingDTOApi = project("starling.dto.api") dependsOn(titanReturnTypes :: utils :: trademgmtModelDeps : _*)
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
lazy val instrument = project("instrument") dependsOn (curves, titanReturnTypes)
lazy val reportsFacility = project("reports.facility") dependsOn guiapi
lazy val rabbitEventViewerApi = project("rabbit.event.viewer.api") dependsOn(pivot, guiapi)
lazy val tradeFacility = project("trade.facility") dependsOn guiapi
lazy val gui = project("gui") dependsOn (fc2Facility, tradeFacility, reportsFacility, browser, rabbitEventViewerApi, singleClasspathManager)
lazy val starlingClient = project("starling.client") dependsOn (starlingDTOApi, bouncyrmi)
lazy val dbx = project("dbx") dependsOn (utils, props)
lazy val schemaevolution = project("schemaevolution") dependsOn (dbx, utils)
lazy val databases = project("databases") dependsOn (pivot, concurrent, starlingDTOApi, dbx, instrument)
lazy val titan = project("titan") dependsOn databases
lazy val services = project("services").withResourceDirs("resources", "test-resources") dependsOn (curves, concurrent, loopyxl, titan, gui, titanReturnTypes, schemaevolution)
lazy val rabbitEventViewerService = project("rabbit.event.viewer.service") dependsOn (rabbitEventViewerApi, databases, services)
lazy val tradeImpl = project("trade.impl") dependsOn (services, tradeFacility)
lazy val oil = project("oil") dependsOn services
lazy val metals = project("metals").withResourceDirs("resources", "test-resources") dependsOn tradeImpl
lazy val reportsImpl = project("reports.impl") dependsOn services

val hostTitanComponents = true
val titanEnvAppServerLibs : List[File] = if (hostTitanComponents) file("webservice", "lib-jboss") :: Nil else Nil

lazy val webservice = {
  lazy val name = "webservice"
  lazy val libs = file(".maker/scala-lib") :: List("lib_managed", "lib", "maker-lib").map(file(name, _)) ::: titanEnvAppServerLibs
  lazy val resources =  List(file(name, "resources"), file(name, "test-resources"))
  val root = file(name)
  new Project(
    name,
    root,
    sourceDirs = file(root, "src") :: Nil,
    tstDirs = file(root, "test") :: Nil,
    libDirs = libs,
    resDirs = resources,
    props = makerProps
  ) dependsOn (utils :: manager :: props :: daterange :: starlingDTOApi :: quantity :: instrument :: (if (hostTitanComponents) logisticsModelDeps ::: trademgmtModelDeps else Nil) : _*)
}

lazy val startserver = project("startserver") dependsOn (reportsImpl, metals, oil, starlingClient, webservice, rabbitEventViewerService)
lazy val launcher = project("launcher") dependsOn (startserver, booter)
lazy val starling = new TopLevelProject("starling", List(launcher), makerProps, List(ProjectLib(manager.name, true)))

def runLauncher = {
  launcher.compile
  launcher.runMain(
    "starling.launcher.Launcher")(
    commonLaunchArgs : _*)()
}

def runServer = {
  launcher.compile
  launcher.runMain(
    "starling.startserver.Server")(
    commonLaunchArgs : _*)()
}

def writeClasspath {
  val cp = launcher.compilationClasspath
  writeToFile(file("launcher-classpath.sh"), "export STARLING_CLASSPATH=" + cp)
}
