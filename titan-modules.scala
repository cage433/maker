println("\n ** Loading Titan Modules Build...\n")


/**
 * titan component builds and helper utils
 */
lazy val titanBinDeps = {
  lazy val name = "titan.bindeps"
  new Project(
    name,
    file(name),
    managedLibDirName = "lib_managed",
    ivySettingsFile = file(name, "maker-ivysettings.xml"))
}

// build a standard titan component (module) webapp  definition
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
    providedDirs = List(file(titanService, "../../.maker/lib")),
    managedLibDirName = "lib_managed",
    resourceDirs = List(file(titanService, "src/main/resources")),
    props = makerProps,
    ivySettingsFile = file(titanService, "../../.maker/ivy/maker-ivysettings.xml"),
    webAppDir = Some(file(titanService, "src/main/webapp"))
  )
}

// titan components we can potentially build from sources
lazy val titanConfig = projectT("configuration")
lazy val titanMurdoch = projectT("murdoch").dependsOn(trademgmtModelDeps : _*)
lazy val titanTradeService = projectT("tradeservice") dependsOn(trademgmtModelDeps : _*)
lazy val titanPermission = projectT("permission")
lazy val titanReferenceData = projectT("referencedata") dependsOn(trademgmtModelDeps : _*)
lazy val titanLogistics = projectT("logistics").dependsOn(logisticsModelDeps ::: trademgmtModelDeps : _*)
lazy val titanInvoicing = projectT("invoicing").withAdditionalSourceDirs(List("target/generated-sources/")).dependsOn(starlingClient :: trademgmtModelDeps : _*)
lazy val titanCostsAndIncomes = projectT("costsandincomes").dependsOn(starlingClient :: daterange :: quantity :: starlingDTOApi :: trademgmtModelDeps : _*)
lazy val titanMtmPnl = projectT("mtmpnl").dependsOn(starlingClient :: trademgmtModelDeps : _*)
lazy val titanReferenceDataNew = projectT("referencedatanew")
lazy val titanMapping = projectT("mapping")
lazy val titanSecuritisation = projectT("securitisation") dependsOn starlingClient

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
                titanInvoicing)

// list of components to take from binaries
lazy val titanBinDepComponentList = starlingProperties.getProperty("TitanProxiedServices").split(":").toList.map(_.toLowerCase)

// list of titan projects to build from sources 
lazy val titanComponents = allTitanComponents.filterNot(p => titanBinDepComponentList.exists(c => p.name.contains(c)))

lazy val starlingTitanDeps = project("titanComponents") dependsOn (titanComponents : _*)

lazy val titanBuilder = project("titan.builder").dependsOn(launcher, starlingTitanDeps)
lazy val titanLauncher = project("titan.launcher").dependsOn(launcher)

import maker.task.BuildResult
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
  val failures = titanComponents.map(p => p.pack.res).collect{ case e @ Left(_) => e }
  failures match {
    case Nil  =>
      println("Titan deploy to: " + deployDir.getAbsolutePath)
      titanComponents.foreach(deployWar(deployDir))
    case r @ _ => println("failed to build "); r
  }
}
def deployToTitanJboss = deployWarsTo(jbossDeployDir)
def deployToTitanJetty = deployWarsTo(jettyDeployDir)

// build all of startling and titan dependencies for starling
// compile and package the titan web apps
// deploy them to local jboss
def buildAndDeployWithTitanJboss = buildWithTitan.map(_ => deployToTitanJboss)
def buildAndDeployWithTitanJetty = buildWithTitan.map(_ => deployToTitanJetty)

def deployTitanJbossWars {
  titanBinDeps.update
  val titanBinDepsDir = titanBinDeps.managedLibDir
  val availableBinDeps = titanBinDepsDir.listFiles.filter(f => f.getName.endsWith(".war"))
  println("Available bin deps: " + availableBinDeps.mkString(","))
  val filesToCopyToJboss = availableBinDeps.filter(f => titanBinDepComponentList.exists(c => f.getName.toLowerCase.contains(c.toLowerCase)))
  println("copying files to " + jbossDeployDir.getAbsolutePath + ", " + filesToCopyToJboss.mkString(","))
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
      "-Dtitan.webapp.server.log=logs/titan-server.log" :: (if (debug) 
      "-Xdebug" :: "-Xrunjdwp:transport=dt_socket,server=y,address=6666" :: Nil else Nil) :::
    commonLaunchArgs.toList) : _*)()
}
def runStarlingWithTitan : Unit = runStarlingWithTitanDeploy()

println("\n ** (Type helpTitan for a list of common Titan commands) ** \n")

