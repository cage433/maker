println("\n ** Loading Titan Modules Build...\n")


/**
 * titan component builds and helper utils
 */
lazy val titanBinDeps = {
  lazy val name = "titan.bindeps"
  new Project(
    name,
    file(name),
    managedLibDirName = "lib_managed"
  )
}

// for inverted (regular JavaSE) combined classpath,
// pull some common stuff out of packages wars (inner classpath) into the outer (parent classpath) environment
lazy val additionalTitanLibraryExclusions = List(
  "commons-httpclient" % "commons-httpclient",
  "org.apache.httpcomponents" % "httpcore",
  "org.jboss.resteasy" % "resteasy-jaxrs",
  "com.oracle" % "ojdbc6",
  "org.jboss.resteasy" % "jaxrs-api",
  "org.slf4j" % "slf4j-api",
  "xml-apis" % "xml-apis",
  "org.scalatest" % "scalatest_2.8.1" // less than ideal but titan common libs refer to scala 2.8.1 version of scalatest which can case a runtime class cast exception when running tests. Luckily it has a crossed artifact id so it can be eliminated specifically
)

// as above but to exclude from the packaging explicitly, by name
lazy val classpathProvidedLibs = additionalTitanLibraryExclusions.map(_.artifactId.id)

// shared cost and incomes lib that contains some common and test classes necessary to run c&i module unit tests
lazy val titanCostsAndIncomesLib = {
  val root = file("../../lib/costsandincomes/internal")
  new Project(
    "costsandincomes", 
    root,
    sourceDirs = List(file(root, "src/main/scala")),
    tstDirs = List(file(root, "src/test/scala")),
    libDirs = List(file(root, "lib_managed"),
      file(root, "lib"),
      file(root, ".maker/scala-lib")),
    managedLibDirName = "lib_managed",
    resourceDirs = List(file(root, "src/main/resources")),
    props = makerProps,
    ivySettingsFile = file(root, "../../../services/.maker/ivy/ivysettings.xml"),
    moduleIdentity = Some("com.trafigura.titan.shared-libs" % "costsandincomes-internal"),
    additionalLibs = List("com.oracle" % "ojdbc6" % "11.2.0.1.0"),
    additionalExcludedLibs = additionalTitanLibraryExclusions.filterNot(_.groupId.id == "com.oracle"), // dependency on oracle lib here is test scope only, redo once we support proper scoping/configs
    providedLibs = classpathProvidedLibs
  ) dependsOn (starlingDTOApi, daterange, quantity)
}

// build a standard titan component (module) webapp  definition,
//   but with classpath inversion considerations...
def projectT(name : String) = {
  lazy val titanService = "../" + name + "/service"
  new Project(
    name, 
    file(titanService),
    sourceDirs = List(file(titanService, "src/main/scala")),
    tstDirs = List(file(titanService, "src/test/scala")),
    libDirs = List(file(titanService, "lib_managed"),
      file(titanService, "lib"),
      file(titanService, ".maker/scala-lib")),
//    providedLibDirs = List(file(titanService, "../../.maker/lib")),
    managedLibDirName = "lib_managed",
    resourceDirs = List(file(titanService, "src/main/resources")),
    props = makerProps,
    ivySettingsFile = file(titanService, "../../.maker/ivy/ivysettings.xml"),
    webAppDir = Some(file(titanService, "src/main/webapp")),
    additionalExcludedLibs = additionalTitanLibraryExclusions,
    providedLibs = classpathProvidedLibs
  )
}

// titan components we can potentially build from sources
lazy val titanConfig = projectT("configuration")
lazy val titanMurdoch = projectT("murdoch").dependsOn(trademgmtModelDeps : _*)
lazy val titanTradeService = projectT("tradeservice") dependsOn(trademgmtModelDeps : _*)
lazy val titanPermission = projectT("permission")
lazy val titanReferenceData = projectT("referencedata") dependsOn(trademgmtModelDeps : _*)
lazy val titanLogistics = projectT("logistics").dependsOn(logisticsModelDeps ::: trademgmtModelDeps : _*)
lazy val titanInvoicing = projectT("invoicing").withAdditionalSourceDirs("target/generated-sources/").setAdditionalExcludedLibs().withProvidedLibs(classpathProvidedLibs : _*).dependsOn(starlingClient :: trademgmtModelDeps : _*)
lazy val titanCostsAndIncomes = projectT("costsandincomes")/*.withAdditionalTestDirs("../../../lib/costsandincomes/internal/src/test/scala").withAdditionalLibs("com.trafigura.titan.shared-libs" % "costsandincomes-internal" % "2.7.2")*/.dependsOn(/* titanCostsAndIncomesLib :: */ starlingClient :: daterange :: quantity :: starlingDTOApi :: trademgmtModelDeps : _*)
lazy val titanMtmPnl = projectT("mtmpnl").dependsOn(titanCostsAndIncomesLib :: starlingClient :: trademgmtModelDeps : _*)
lazy val titanReferenceDataNew = projectT("referencedatanew")
lazy val titanMapping = projectT("mapping")
lazy val titanSecuritisation = projectT("securitisation") dependsOn starlingClient
lazy val titanFinance= projectT("finance") dependsOn starlingClient

// all titan components that can be built from sources as part of an integrated build
lazy val allTitanComponents = Seq(
//                titanConfig,      // not on master
//                titanPermission,  // not on master
                titanMurdoch,
                titanTradeService,
                titanReferenceData,
                titanLogistics,
                titanCostsAndIncomes,
                titanMtmPnl,
                titanInvoicing
//                titanFinance      // this builds ok but is excluded until the runtime env is sorted out
)

// list of components to take from binaries

lazy val titanBinDepComponentList : List[String] = Option(starlingProperties.getProperty("TitanProxiedServices")).map(_.split(":").toList.map(_.toLowerCase)).getOrElse(Nil)

// list of titan projects to build from sources 
lazy val titanComponents = allTitanComponents.filterNot(p => titanBinDepComponentList.exists(c => p.name.contains(c)))

lazy val starlingTitanDeps = project("titanComponents") dependsOn (titanComponents : _*)

// builder and launcher are split up for the purposes of separating run time scope from compile time,
// when we get to add scopes to maker properly these two projects can be combined
lazy val titanBuilder = project("titan.builder").dependsOn(launcher, starlingTitanDeps)
lazy val titanLauncher = project("titan.launcher").dependsOn(launcher)

def buildWithTitan = {
  titanLauncher.compile
  starlingTitanDeps.pack.res match {
    case r @ Right(_) => Some(r)
    case _ => None
  }
}
lazy val jbossDeployDir = file(System.getenv("JBOSS_HOME") + "/server/trafigura/deploy")
lazy val jettyDeployDir = file("titan.deploy/wars")
def deployWar(deployDir : File)(project : Project) {
  copyFileToDirectory(file(project.root, "package/" + project.name + ".war"), deployDir)
}
lazy val deployWarToJetty = deployWar(jettyDeployDir) _
lazy val deployWarToJboss = deployWar(jbossDeployDir) _
def deployWarsTo(deployDir : File) = {
  println("building and packaging: " + titanComponents.map(_.name).mkString(","))
  val failures = titanComponents.map(p => p.packOnly.res).collect{ case e @ Left(_) => e }
  failures match {
    case Nil  =>
      println("Titan deploy to: " + deployDir.getAbsolutePath)
      titanComponents.foreach(deployWar(deployDir))
    case r @ _ => println("failed to build "); r
  }
}
def deployToTitanJboss = deployWarsTo(jbossDeployDir)
def deployToTitanJetty = deployWarsTo(jettyDeployDir)

/**
 * build all of startling and titan dependencies for starling
 * compile and package the titan web apps
 * deploy them to local jboss
 */
def buildAndDeployWithTitanJboss = buildWithTitan.map(_ => deployToTitanJboss)
def buildAndDeployWithTitanJetty = buildWithTitan.map(_ => deployToTitanJetty)

def deployTitanJbossWars {
  titanBinDeps.update
  val titanBinDepsDir = titanBinDeps.managedLibDir
  val availableBinDeps = titanBinDepsDir.listFiles.filter(f => f.getName.endsWith(".war"))
  println("Available bin deps: " + availableBinDeps.mkString(","))
  val filesToCopyToJboss = availableBinDeps.filter(f => titanBinDepComponentList.exists(c => f.getName.toLowerCase.contains(c.toLowerCase)))
  println("Copying files to " + jbossDeployDir.getAbsolutePath + ", " + filesToCopyToJboss.mkString(","))
  filesToCopyToJboss.foreach(f => copyFileToDirectory(f, jbossDeployDir))
}

def runStarlingWithTitanDeploy(deployWars : Boolean = true, debug : Boolean = false) {
  val titanProxiedComponents = titanBinDepComponentList.mkString(":")
  val titanProxyHost = starlingProperties.getProperty("TitanProxiedServiceHost")
  val titanProxyPort = starlingProperties.getProperty("TitanProxiedServicePort")
  println("***** Titan Proxied Components = " + titanProxiedComponents)
  titanBuilder.compile
  if (deployWars) deployToTitanJetty
  titanLauncher.runMain(
    "starling.launcher.DevLauncher")(
    ( "-Djavax.xml.parsers.DocumentBuilderFactory=com.sun.org.apache.xerces.internal.jaxp.DocumentBuilderFactoryImpl" ::
      "-Dstarling.titan.enabled=true" ::
      "-Dtitan.webapp.server.log=logs/titan-server.log" :: (if (debug) 
      "-Xdebug" :: "-Xrunjdwp:transport=dt_socket,server=y,address=6666" :: Nil else Nil) :::
    commonLaunchArgs.toList) : _*)()
}
def runStarlingWithTitan : Unit = runStarlingWithTitanDeploy()

println("\n ** (Type helpTitan for a list of common Titan commands) ** \n")

