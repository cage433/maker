import _root_.sbt._
import _root_.sbt._
import _root_.sbt.IvyActions
import _root_.sbt.IvySbt
import _root_.sbt.Logger
import _root_.sbt.MakePom
import sbt._
import Keys._
import java.io.File


object StarlingBuild extends Build{

  val projectName = "starling"
  val scalaVer = "2.9.1"
  val amqpVersion = "1.7.2"

  import Utils._

  val starlingVersion = {
    val r = System.getProperty("build.number")
    if (r == null) "SNAPSHOT" else r
  }

  val verboseMode = if (Option(System.getProperty("verbose")).isDefined) true else false

  val ivyUpdateLogging = if (verboseMode) ivyLoggingLevel := UpdateLogging.Full else ivyLoggingLevel := UpdateLogging.Quiet

  println("")
  println("This is the build number: " + starlingVersion)
  println("")

  def lib_managed_jars(base : File) : Seq[Attributed[File]] = (((base / "lib_managed") ** "*.jar")).getFiles.map{f : File => Attributed.blank(f)}
  lazy val standardSettings = Defaults.defaultSettings ++ Seq(
    unmanagedSourceDirectories in Compile <+= baseDirectory(_/"src"),
    unmanagedSourceDirectories in Test <+= baseDirectory(_/"tests"),
    unmanagedResourceDirectories in Test <+= baseDirectory(_/"test-resources"),
    unmanagedResourceDirectories in Compile <+= baseDirectory(_/"resources"),
    unmanagedBase <<= baseDirectory( (base: File) => base /"lib"),
    unmanagedClasspath in Test <+= (baseDirectory) map { bd => Attributed.blank(bd / "resources") },
    unmanagedJars in Compile <++= (baseDirectory) map lib_managed_jars,
    unmanagedJars in Test <++= (baseDirectory) map lib_managed_jars,
    unmanagedJars in Runtime <++= (baseDirectory) map lib_managed_jars,
    ivyXML := <dependencies><exclude artifact="jcl-over-slf4j"/><exclude artifact="junit"/></dependencies>, 
    scalaVersion := scalaVer,
    showLibsTask,
    writeClasspathScriptTask,
    credentialsSetting,
    publishSetting,
    resolvers += "Non-Trafigura Public Repositories" at "http://nexus.global.trafigura.com:8081/nexus/content/groups/mirror/",
    resolvers += "trafigura" at "http://nexus.global.trafigura.com:8081/nexus/content/repositories/tooling-releases/",
    organizationName := "Trafigura",
    version := starlingVersion,
    shellPrompt  := ShellPrompt.buildShellPrompt
  )

  def overrideMakePom(module: IvySbt#Module, configuration: MakePomConfiguration, log: Logger) {
		import configuration.{allRepositories, moduleInfo, configurations, extra, file, filterRepositories, process}

    class OverrideMakePom extends MakePom(log) {
      override def isValidIDCharacter(c: Char) = true
    }

		module.withModule(log) { (ivy, md, default) =>
			(new OverrideMakePom()).write(ivy, md, moduleInfo, configurations, extra, process, filterRepositories, allRepositories, file)
			log.info("Wrote " + file.getAbsolutePath)
		}
	}

  lazy val standardSettingsNexus = Defaults.defaultSettings ++ Seq(
    makePom <<= (ivyModule, makePomConfiguration, streams) map { (module, config, s) => overrideMakePom(module, config, s.log); config.file },
    unmanagedSourceDirectories in Compile <+= baseDirectory(_/"src"),
    unmanagedSourceDirectories in Test <+= baseDirectory(_/"tests"),
    unmanagedResourceDirectories in Test <+= baseDirectory(_/"test-resources"),
    unmanagedResourceDirectories in Compile <+= baseDirectory(_/"resources"),
    unmanagedClasspath in Test <+= (baseDirectory) map { bd => Attributed.blank(bd / "resources") },
    ivyXML := <dependencies><exclude artifact="jcl-over-slf4j"/><exclude artifact="junit"/></dependencies>,
    scalaVersion := scalaVer,
    showLibsTask,
    writeClasspathScriptTask,
    deployClasspathTask,
    credentialsSetting,
    publishSetting,
    resolvers += "Non-Trafigura Public Repositories" at "http://nexus.global.trafigura.com:8081/nexus/content/groups/mirror/",
    resolvers += "trafigura" at "http://nexus.global.trafigura.com:8081/nexus/content/repositories/tooling-releases/",
    resolvers += "titan-cross-stream-snapshots" at "http://nexus.global.trafigura.com:8081/nexus/content/repositories/titan-cross-stream-snapshots/",
    resolvers += "titan-cross-stream-releases" at "http://nexus.global.trafigura.com:8081/nexus/content/repositories/titan-cross-stream-releases/",
    resolvers += "tooling-snapshots" at "http://nexus.global.trafigura.com:8081/nexus/content/repositories/tooling-snapshots/",
    resolvers += "trademgmt-bindeps-releases" at "http://nexus.global.trafigura.com:8081/nexus/content/repositories/trademgmt-bindeps-releases/",
    resolvers += "trademgmt-bindeps-snapshots" at "http://nexus.global.trafigura.com:8081/nexus/content/repositories/trademgmt-bindeps-snapshots/",
    resolvers += "logistics-bindeps-releases" at "http://nexus.global.trafigura.com:8081/nexus/content/repositories/logistics-bindeps-releases/",
    resolvers += "logistics-bindeps-snapshots" at "http://nexus.global.trafigura.com:8081/nexus/content/repositories/logistics-bindeps-snapshots/",
    resolvers += "referencedata-bindeps-releases" at "http://nexus.global.trafigura.com:8081/nexus/content/repositories/referencedata-bindeps-releases/",
    resolvers += "referencedata-bindeps-snapshots" at "http://nexus.global.trafigura.com:8081/nexus/content/repositories/referencedata-bindeps-snapshots/",
    ivyUpdateLogging,
    //resolvers += Resolver.defaultLocal,
    organizationName := "Trafigura",
    version := starlingVersion,
    shellPrompt  := ShellPrompt.buildShellPrompt
  )

  lazy val publishSetting = publishTo <<= (version) {

    val isSnapshot = starlingVersion.trim.toLowerCase.endsWith("snapshot")

    version : String =>
      def repo(name: String) = {
        if (isSnapshot) {
          name at "http://nexus.global.trafigura.com:8081/nexus/content/repositories/starling-test/" + name
        } else {
          name at "http://nexus.global.trafigura.com:8081/nexus/content/repositories/starling-releases/" + name
        }
      }

    val repoName = projectName + (if (isSnapshot) /* "-snapshots" */ "-releases" else "-releases")

    Some(repo(repoName))
  }

  lazy val credentialsSetting = credentials += {
    /*Seq("admin", "admin123").map(k => Option(System.getProperty(k))) match {
      case Seq(Some(user), Some(pass)) =>
        Credentials("Sonatype Nexus Repository Manager", "nexus-direct.scala-tools.org", user, pass)
      case _ =>
        Credentials(Path.userHome / ".ivy2" / ".credentials")
    }*/
    Credentials("Sonatype Nexus Repository Manager", "nexus.global.trafigura.com", "deployment", "depl0yment")
  }


  val testDependency = "compile;test->test"

  val managerDependencies = Seq(
    "org.scala-lang" % "scala-swing" % "2.9.1" /*/*withSources()*/*/,
    "cglib" % "cglib-nodep" % "2.2"
  )

  lazy val manager = Project(
    "manager",
    file("./manager"),
    settings = standardSettingsNexus  ++ Seq(libraryDependencies ++= managerDependencies)
  ) 

  val utilsDependencies = Seq(
    "net.liftweb" % "lift-json_2.9.0" % "2.4-M2",
    "cglib" % "cglib-nodep" % "2.2" withSources(),
    "joda-time" % "joda-time" % "1.6" withSources(),
    "com.rabbitmq" % "amqp-client" % amqpVersion withSources(),
    "log4j" % "log4j" % "1.2.16" withSources(),
    "org.slf4j" % "slf4j-log4j12" % "1.6.1" withSources(),
    "com.google.collections" % "google-collections" % "1.0" withSources(),
    "commons-codec" % "commons-codec" % "1.5" withSources(),
    "commons-io" % "commons-io" % "1.3.2" withSources(),
    "colt" % "colt" % "1.0.3",
    "com.thoughtworks.xstream" % "xstream" % "1.3.1" withSources(),
    "org.testng" % "testng" % "6.2.1",
    "com.google.inject" % "guice" % "2.0",
    "org.scala-tools" %% "scala-stm" % "0.3",
    "org.scala-lang" % "scala-swing" % "2.9.1" withSources(),
    "org.scalaz" %% "scalaz-core" % "6.0.3" withSources(),
    "spy" % "spymemcached" % "2.7.3" withSources(),
    "org.scalatest" %% "scalatest" % "1.6.1" withSources(),
    "org.mockito" % "mockito-all" % "1.8.2",
    "org.jmock" % "jmock" % "2.5.1"
  )

  lazy val utils = Project(
    "utils", 
    file("./utils"), 
    settings = standardSettingsNexus ++ Seq(libraryDependencies ++= utilsDependencies)
  )

  // Not uploading this to nexus at the moment.
  lazy val osgiRun = Project(
    "osgirun",
    file("./osgirun"),
    settings = standardSettings
  )

  lazy val booter = Project(
    "booter",
    file("./booter"),
    settings = standardSettingsNexus
  )

  lazy val concurrent = Project(
    "concurrent", 
    file("./concurrent"),
    settings = standardSettingsNexus
  ) dependsOn (utils) 

  lazy val quantity = Project(
    "quantity", 
    file("./quantity"),
    settings = standardSettingsNexus
  ) dependsOn (utils)

  val osgiManagerDependencies = Seq(
    "org.osgi" % "org.osgi.compendium" % "4.2.0",
    "org.osgi" % "org.osgi.core" % "4.2.0"
  )

  lazy val osgiManager = Project(
    "osgimanager",
    file("./osgimanager"),
    settings = standardSettingsNexus ++  Seq(libraryDependencies ++= osgiManagerDependencies)
  ) dependsOn(manager, utils)

  lazy val singleClasspathManager = Project(
    "singleclasspathmanager",
    file("./singleclasspathmanager"),
    settings = standardSettingsNexus
  ) dependsOn(manager, utils, osgiManager)

  lazy val pivot = Project(
    "pivot", 
    file("./pivot"),
    settings = standardSettingsNexus
  ) dependsOn(quantity)

  lazy val pivotUtils = Project(
    "pivot.utils", 
    file("./pivot.utils"),
    settings = standardSettingsNexus
  ) dependsOn(daterange, pivot)

  lazy val daterange = Project(
    "daterange", 
    file("./daterange"),
    settings = standardSettingsNexus
  ) dependsOn(utils)


  lazy val titanReturnTypes = Project(
    "titan-return-types",
    file("./titan.return.types"),
    settings = standardSettingsNexus
  ) dependsOn(daterange, quantity, utils)

  val mathsDependencies = Seq(
    "org.apache.commons" % "commons-math" % "2.1"
  )

  lazy val maths = Project(
    "maths",
    file("./maths"),
    settings = standardSettingsNexus ++ (libraryDependencies ++= mathsDependencies)
  ) dependsOn(quantity % testDependency, daterange % testDependency)

  val starlingApiDependencies = Seq(
    "com.trafigura.titan" % "model-logistics-public-scala-bindings" % "1.2-SNAPSHOT",
    "com.trafigura.titan" % "model-trademgmt-public-scala-bindings" % "1.1",
    "org.slf4j" % "slf4j-api" % "1.6.1",
    "dom4j" % "dom4j" % "1.6.1",
    "com.rabbitmq" % "amqp-client" % amqpVersion,
    "joda-time" % "joda-time" % "1.6",
    "org.codehaus.jettison" % "jettison" % "1.1",
    "commons-httpclient" % "commons-httpclient" % "3.1",
    "com.trafigura.tradinghub" % "scala-hub-support" % "2.17",
    "com.trafigura.tradinghub" % "persistence-support" % "2.17",
    "org.jboss.resteasy" % "jaxrs-api" % "2.2.2.GA"
  )

  lazy val starlingApi = Project(
    "starling-api",
    file("./starling.api"),
    settings = standardSettingsNexus ++ Seq(libraryDependencies ++= starlingApiDependencies)
  ) dependsOn(titanReturnTypes)

  lazy val props = Project(
    "props",
    file("./props"),
    settings = standardSettingsNexus
  ) dependsOn(utils, manager)

  val authDependencies = Seq(
//    "net.java.dev.jna" % "jna" % "3.3.0", Put this back in once sbt can handle classifiers properly
    "sbt.bug.jna" % "jna" % "3.3.0",
    "net.java.dev.jna" % "jna" % "3.3.0" classifier "platform"
  
  )

  lazy val auth = Project(
    "auth", 
    file("./auth"),
    settings = standardSettingsNexus ++ Seq(libraryDependencies ++= authDependencies)
//    settings = standardSettingsNexus ++ Seq(ivyXML := authIvyXML)
  ) dependsOn (utils, manager, props)

  val bouncyrmiDependencies = Seq(
    "cglib" % "cglib-nodep" % "2.2",
    "org.jboss.netty" % "netty" % "3.2.5.Final",
    "commons-io" % "commons-io" % "1.3.2",
    "commons-logging" % "commons-logging" % "1.1.1"
  )

  lazy val bouncyrmi = Project(
    "bouncyrmi", 
    file("./bouncyrmi"),
    settings = standardSettingsNexus ++ Seq(libraryDependencies ++= bouncyrmiDependencies)
  ) dependsOn(manager, auth, props)

  val loopyxlDependencies = Seq(
    "com.google.protobuf" % "protobuf-java" % "2.3.0"
  )

  lazy val loopyxl = Project(
    "loopyxl", 
    file("./loopyxl"),
    settings = standardSettingsNexus ++ Seq(libraryDependencies ++= loopyxlDependencies)
  ) dependsOn(manager, auth)

  lazy val browserService = Project(
    "browser-service",
    file("./browser.service"),
    settings = standardSettingsNexus
  ) dependsOn(manager)

  val browserDependencies = Seq(
    "com.thoughtworks.xstream" % "xstream" % "1.3.1",
    "com.google.collections" % "google-collections" % "1.0",
    "jxlayer" % "jxlayer" % "4.0",
    "jgoodies" % "looks" % "2.3.1",
    "org.swinglabs" % "swingx-core" % "1.6.2-2" withSources(),
    "mig-swing" % "miglayout" % "4.0",
    "net.java.dev.timingframework" % "timingframework" % "1.0",
    "transloader" % "transloader" % "0.4",
    "starling-external-jars" % "org.eclipse.mylyn.wikitext.core" % "1.4" classifier "e3x",
    "starling-external-jars" % "org.eclipse.mylyn.wikitext.textile.core" % "1.4"
  )

  lazy val browser = Project(
    "browser",
    file("./browser"),
    settings = standardSettingsNexus ++ (libraryDependencies ++= browserDependencies)
  ) dependsOn(browserService, manager)

  val guiapiDependencies = Seq(
    "net.debasishg" % "sjson_2.8.0" % "0.8",
    "net.databinder" % "dispatch-json_2.8.0" % "0.7.4"
  )

  lazy val guiapi = Project(
    "gui-api",
    file("./gui.api"),
    settings = standardSettingsNexus ++ (libraryDependencies ++= guiapiDependencies)
  ) dependsOn(pivotUtils, quantity, auth, bouncyrmi, browserService, manager)

  lazy val fc2Facility = Project(
    "fc2-facility",
    file("./fc2.facility"),
    settings = standardSettingsNexus
  ) dependsOn(daterange, guiapi)

  lazy val curves = Project(
    "curves", 
    file("./curves"),
    settings = standardSettingsNexus
  ) dependsOn(utils % testDependency, daterange % testDependency, maths, pivotUtils, guiapi, quantity % testDependency)

  lazy val instrument = Project(
    "instrument", 
    file("./instrument"),
    settings = standardSettingsNexus
  ) dependsOn(curves % testDependency, daterange % testDependency, titanReturnTypes)

  lazy val reportsFacility = Project(
    "reports-facility",
    file("./reports.facility"),
    settings = standardSettingsNexus
  ) dependsOn(guiapi)

  lazy val rabbitEventViewerApi = Project(
    "rabbit-event-viewer-api",
    file("./rabbit.event.viewer.api"),
    settings = standardSettingsNexus
  ) dependsOn(pivot, manager)

  val guiDependencies = Seq(
    "jfree" % "jfreechart" % "1.0.0",
    "jfree" % "jcommon" % "1.0.0",
    "javax.servlet" % "servlet-api" % "2.5"
  )

  lazy val gui = Project(
    "gui", 
    file("./gui"),
    settings = standardSettingsNexus ++ (libraryDependencies ++= guiDependencies)
  ) dependsOn(fc2Facility, tradeFacility, reportsFacility, browser, rabbitEventViewerApi, singleClasspathManager)

  lazy val tradeFacility = Project(
    "trade", 
    file("./trade.facility"),
    settings = standardSettingsNexus
  ) dependsOn(auth, guiapi, manager)

 
  lazy val starlingClient = Project(
    "starling-client",
    file("./starling.client"),
    settings = standardSettingsNexus ++ Seq(libraryDependencies ++= starlingApiDependencies)
  ) dependsOn(starlingApi, bouncyrmi)

  val dbxDependencies = Seq(
    "com.jolbox" % "bonecp" % "0.7.1.RELEASE",
    "org.springframework" % "spring-jdbc" % "3.0.5.RELEASE",
    "jtds" % "jtds" % "1.2.5",
    "com.oracle" % "ojdbc6" % "11.2.0.1.0"
  )

  lazy val dbx = Project(
    "dbx",
    file("./dbx"),
    settings = standardSettingsNexus ++ (libraryDependencies ++= dbxDependencies)
  ) dependsOn(manager, utils, instrument)

  val databaseDependencies = Seq(
    "org.slf4j" % "slf4j-api" % "1.6.1",
    "org.scala-tools.testing" %% "scalacheck" % "1.9",
    "org.apache.derby" % "derby" % "10.5.3.0_1",
    "hsqldb" % "hsqldb" % "1.8.0.10"/*  conf="default->master"*/,
    "com.h2database" % "h2" % "1.2.131",
    "org.springframework" % "spring-tx" % "3.0.5.RELEASE",
    "org.springframework" % "spring-core" % "3.0.5.RELEASE",
    "org.springframework" % "spring-beans" % "3.0.5.RELEASE",
    "commons-logging" % "commons-logging" % "1.1.1",
    "starling-external-jars" % "mimapi" % "2.2.0",
    "org.acplt" % "oncrpc" % "1.0.7"
  )

  lazy val databases = Project(
    "databases", 
    file("./databases"),
    settings = standardSettingsNexus ++  (libraryDependencies ++= databaseDependencies)
  ) dependsOn(curves % "test->test", pivot , guiapi , concurrent , auth , starlingApi, dbx, props)

  lazy val rabbitEventViewerService = Project(
    "rabbit-event-viewer-service",
    file("./rabbit.event.viewer.service"),
    settings = standardSettingsNexus
  ) dependsOn(rabbitEventViewerApi, pivot, databases, manager)

  lazy val titan = Project(
				"titan",
				file("./titan"),
				settings = standardSettingsNexus ++ Seq(libraryDependencies ++= starlingApiDependencies)
			) dependsOn(curves % "test->test", databases)

  val servicesDependencies = Seq(
    "javax.mail" % "mail" % "1.4",
    "org.mortbay.jetty" % "jetty" % "6.1.26",
    "org.mortbay.jetty" % "jetty-util" % "6.1.26",
    "org.subethamail" % "subethasmtp-wiser" % "1.2",
    "org.subethamail" % "subethasmtp-smtp" % "1.2",
    "org.springframework" % "spring-context-support" % "3.0.5.RELEASE",
    "com.thoughtworks.paranamer" % "paranamer" % "2.3",
    "starling-external-jars" % "xlloop" % "0.3.1",
    "commons-httpclient" % "commons-httpclient" % "3.1",
    "org.jboss.resteasy" % "jaxrs-api" % "2.2.2.GA",
    "org.jboss.resteasy" % "resteasy-jaxrs" % "2.2.2.GA",
    "org.scannotation" % "scannotation" % "1.0.3",
    "javax.servlet" % "servlet-api" % "2.5",
  
    "com.trafigura.titan.shared-libs" % "titan-core" % "1.1" notTransitive(),
    "com.trafigura.titan.shared-libs" % "titan-security" % "1.1" notTransitive(),
    "com.trafigura.titan.shared-libs" % "titan-utils" % "1.0-SNAPSHOT" notTransitive()
  )

  lazy val services = Project(
    "services", 
    file("./services"),
    settings = standardSettingsNexus ++ (libraryDependencies ++= servicesDependencies)
  ) dependsOn(curves % "test->test", concurrent, loopyxl, titan, gui, fc2Facility, browser, titanReturnTypes)

  lazy val tradeImpl = Project(
    "trade-impl",
    file("./trade.impl"),
    settings = standardSettingsNexus
  ) dependsOn(services, tradeFacility, manager)

  lazy val metals = Project(
    "metals",
    file("./metals"),
    settings = standardSettingsNexus
  ) dependsOn(services % "compile;test->test", tradeImpl)

  lazy val reportsImpl = Project(
    "reports-impl",
    file("./reports.impl"),
    settings = standardSettingsNexus
  ) dependsOn(services % "compile;test->test")


  lazy val startserver = Project(
    "startserver",
    file("./startserver"),
    settings = standardSettingsNexus
  ) dependsOn(services, reportsImpl, tradeImpl, metals, starlingClient, singleClasspathManager, rabbitEventViewerService, webservice)

  lazy val launcher = Project(
    "launcher", 
    file("./launcher"),
    settings = standardSettingsNexus
  ) dependsOn(startserver, gui, singleClasspathManager, booter)

  val webserviceDependencies = Seq(
    "javax.servlet" % "servlet-api" % "2.5",
    "org.jboss.resteasy" % "jaxrs-api" % "2.2.2.GA",
    "org.mortbay.jetty" % "jetty" % "6.1.26",
    "org.mortbay.jetty" % "jetty-util" % "6.1.26",
    "com.thoughtworks.paranamer" % "paranamer" % "2.3",
    "org.jboss.resteasy" % "resteasy-jaxrs" % "2.2.2.GA",
    "net.databinder" %% "dispatch-http" % "0.8.6" withSources(),
    "net.databinder" %% "dispatch-core" % "0.8.6" withSources(),
    "org.apache.httpcomponents" % "httpclient" % "4.1.2",
    "org.apache.httpcomponents" % "httpcore" % "4.1.2",
    "commons-logging" % "commons-logging" % "1.1.1"
  )

  lazy val webservice = Project(
    "webservice",
    file("./webservice"),
    settings = standardSettingsNexus ++ (libraryDependencies ++= webserviceDependencies)
  ) dependsOn(utils, manager, props, daterange, starlingApi)

  // Evil hack so that I can get a classpath exported including the test-classes of all projects.
  // See bin/write-classpath-script.sh
  lazy val dummy = Project(
    "dummy",
    file("./dummy-sbt-vim-hack"),
    settings = standardSettingsNexus
  ) dependsOn(childProjects.map(_ % "test->test") : _*)

  def otherProjectRefereneces : List[ProjectReference] = List(
    booter,
    utils, 
    bouncyrmi, 
    auth, 
    concurrent, 
    quantity, 
    daterange, 
    loopyxl, 
    maths, 
    pivot, 
    pivotUtils,
    guiapi,
    fc2Facility,
    curves,
    instrument,
    gui,
    browser,
    browserService,
    tradeFacility,
    databases,
    titan,
    services,
    launcher,
    manager,
    osgiManager,
    singleClasspathManager,
    osgiRun,
    dbx,
    startserver,
    titanReturnTypes,
    starlingApi,
    starlingClient,
    webservice,
    metals,
    props,
    rabbitEventViewerApi,
    rabbitEventViewerService,
    reportsFacility,
    reportsImpl,
    tradeFacility,
    tradeImpl
  )

  def childProjects : List[ProjectReference] =  otherProjectRefereneces

  val docProjects : List[ProjectReference] =  List(
    utils, 
    bouncyrmi, 
    auth, 
    concurrent, 
    quantity, 
    daterange, 
    loopyxl, 
    maths, 
    pivot, 
    pivotUtils,
    guiapi,
    fc2Facility,
    curves,
    instrument,
    gui,
    browser,
    browserService,
    tradeFacility,
    databases,
    titan,
    services,
    launcher,
    manager,
    osgiManager,
    singleClasspathManager,
    osgiRun,
    dbx,
    startserver,
    titanReturnTypes,
    starlingApi,
    starlingClient,
    webservice,
    props,
    rabbitEventViewerApi, rabbitEventViewerService, 
    reportsFacility, reportsImpl, tradeImpl, metals,
    tradeFacility)

  val sharedProjects : List[ProjectReference] =  List(
      utils, quantity, daterange, titanReturnTypes, titan, databases, starlingApi)
  
  val allPackagedArtifacts = TaskKey[Seq[Map[Artifact, File]]]("all-packaged-artifacts")
  val allSources           = TaskKey[Seq[Seq[File]]]("all-sources")
  val allSourceDirectories = SettingKey[Seq[Seq[File]]]("all-source-directories")
  
  val docSharedRoot = Project("doc-shared", file("doc.shared"), settings = standardSettingsNexus ++ Seq(
      allSources <<= sharedProjects.map(sources in Compile in _).join, // join: Seq[Task[A]] => Task[Seq[A]]
      //allSourceDirectories <<= childProjects.map(sourceDirectories in Compile in _).join,
      //allPackagedArtifacts <<= childProjects.map(packagedArtifacts in _).join,

      // Combine the sources of other modules to generate Scaladoc and SXR annotated sources
      (sources in Compile) <<= (allSources).map(_.flatten),
      (unmanagedJars in Compile) <<= ((childProjects.map(unmanagedJars in Compile in _).join).map(_.flatten)),

      // Avoid compiling the sources here; we just are after scaladoc.
      (compile in Compile) := inc.Analysis.Empty)
    )
  

  val docAllRoot = Project("doc-all", file("doc.all"), settings = standardSettingsNexus ++ Seq(
      allSources <<= docProjects.map(sources in Compile in _).join, // join: Seq[Task[A]] => Task[Seq[A]]
      //allSourceDirectories <<= childProjects.map(sourceDirectories in Compile in _).join,
      //allPackagedArtifacts <<= childProjects.map(packagedArtifacts in _).join,

      // Combine the sources of other modules to generate Scaladoc and SXR annotated sources
      (sources in Compile) <<= (allSources).map(_.flatten),
      (unmanagedJars in Compile) <<= ((childProjects.map(unmanagedJars in Compile in _).join).map(_.flatten)),
      (managedClasspath in Compile) <<= ((childProjects.map(managedClasspath in Compile in _).join).map(_.flatten)),

      // Avoid compiling the sources here; we just are after scaladoc.
      (compile in Compile) := inc.Analysis.Empty)
    )

  val root = Project("starling", file("."), settings = standardSettingsNexus) aggregate (childProjects : _*)

  /**
   * Some utils to help abstract from the unintuitive SBT DSL, for more "common" cases/patterns
   */
  object Utils {

    // make regular tasks based on a function () => Unit and a cmd name and optional description
    def mkTasks[T : Manifest](ls : List[(() => T, String, String)]) = ls.map(t => mkTask(t._1, t._2, t._3))
    def mkTask[T : Manifest](f : () => T, cmd : String, desc : String = "") = {
      lazy val taskKey = TaskKey[T](cmd, desc)
      taskKey := { f() }
    }
    
    // make regular imput tasks based on a function (List[String]) => Unit and a cmd name and optional description
    // takes all the arguments from console and passes them to the function provided as-is
    def mkInputTasks[T : Manifest](ls : List[(List[String] => T, String, String)]) = ls.map(t => mkInputTask(t._1, t._2, t._3))
    def mkInputTask[T : Manifest](f : List[String] => T, cmd : String, desc : String = "default input task") = {
      val inputTaskKey = InputKey[Unit](cmd, desc)
      
      inputTaskKey <<= inputTask { (argTask : TaskKey[Seq[String]]) => 
        (argTask) map { (args : Seq[String]) =>
          f(args.toList)
        }
      }
    }
    implicit def toInputTask[T : Manifest](t : (List[String] => T, String, String)) : sbt.Project.Setting[sbt.InputTask[Unit]] = mkInputTask(t._1, t._2, t._3)

    // utils to show project classpath libs
    val showLibs = TaskKey[Unit]("show-libs")
    val showLibsTask = showLibs <<= (target, fullClasspath in Runtime) map { (target, cp) =>
      println("Target path is: " + target + "\n")
      println("Full classpath is: " + cp.map(_.data).mkString("\n"))
    }

    // write a classpatch script for dev
    val writeClasspathScript = TaskKey[Unit]("write-classpath")
    val writeClasspathScriptTask = writeClasspathScript <<= (target, fullClasspath in Test) map { (target, cp) =>
      import java.io._
      val file = new PrintWriter(new FileOutputStream(new File("set-classpath.sh")))
      cp.map(_.data).getFiles.toList.foreach(println)
      val resourceDirs = cp.map(_.data).getFiles.toList.map(_.getPath).filter(_.endsWith("/classes")).map{s => s.replace("/classes", "/resources")}
      file.println("export CLASSPATH=" + (cp.map(_.data).getFiles.toList ::: resourceDirs).mkString(":"))
      file.println("export JAVA_OPTS='-server -XX:MaxPermSize=1024m -Xss512k -Xmx6000m'")
      file.close()
      None
    }

    val deployClasspath = TaskKey[Unit]("deploy-classpath")
    val deployClasspathTask = deployClasspath <<= (fullClasspath in Compile) map { cp =>
      import java.io._
      val file = new PrintWriter(new FileOutputStream(new File("bin/deploy-classpath.sh")))

      val workingDirectory = System.getProperty("user.dir") + "/"
      val userHome = System.getProperty("user.home")
      val ivyHome = userHome + "/.ivy2"

      val classpathFiles = cp.map(_.data).getFiles.map(_.getPath).toList.filterNot(p => {
        p.endsWith("/lib/scala-library.jar") || p.endsWith("/lib/scala-compiler.jar")
      })
      val resourcesFiles = classpathFiles.filter(_.endsWith("/classes")).map{s => s.replace("/classes", "/resources")}.toList
      val allPaths = classpathFiles ::: resourcesFiles

      val (externalLibraryJarPaths, starlingPaths) = allPaths.partition(_.startsWith(ivyHome))

      val relativeStarlingPaths = starlingPaths.map(_.replaceFirst(workingDirectory, ""))

      val relativeExternalLibraryJarPaths = externalLibraryJarPaths.map(_.replaceFirst(userHome, "\\$HOME"))

      val allRelativePaths = "lib/scala/lib_managed/scala-library-jar-2.9.1.jar" :: "lib/scala/lib_managed/scala-swing-jar-2.9.1.jar" :: relativeStarlingPaths ::: relativeExternalLibraryJarPaths

      file.println("export CLASSPATH=" + allRelativePaths.mkString(":"))
      file.println("export JAVA_OPTS='-server -XX:MaxPermSize=256 -Xmx6000m'")
                  
      file.close()
      None
    }
  }

  object ShellPrompt {
 
    object devnull extends ProcessLogger {
      def info (s: => String) {}
      def error (s: => String) { }
      def buffer[T] (f: => T): T = f
    }
  
    val current = """\*\s+([^\s]+)""".r
  
    def gitBranches = ("git branch --no-color" lines_! devnull mkString)
  
    val buildShellPrompt = { 
      (state: State) => {
        val currBranch = current findFirstMatchIn gitBranches map (_ group(1)) getOrElse "-"
        val currProject = Project.extract (state).currentProject.id
        "%s::%s>".format (currBranch, currProject)
      }
    }
  }
}

