import sbt._
import Keys._
import java.io.File


object StarlingBuild extends Build{

  import Utils._

  val starlingVersion = {
    val r = System.getProperty("build.number")
    if (r == null) "0.1" else r
  }

  println("")
  println("This is the build number: " + starlingVersion)
  println("")

  val useTitanModelBinaries = {
    if (!new File("props.conf").exists)
      false
    else {
      val serverSettingRegexp = """^ServerType\s*=\s*(\w+)\s*$""".r
      scala.io.Source.fromFile("props.conf").getLines.toList.collect {
        case serverSettingRegexp(serverType) => serverType
      } match {
        case List("FC2") => false
        case _ => true
      }
    }
  }
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
    scalaVersion := "2.9.1",
    showLibsTask,
    writeClasspathScriptTask,
    credentialsSetting,
    publishSetting,
    resolvers += "Non-Trafigura Public Repositories" at "http://nexus.global.trafigura.com:8081/nexus/content/groups/mirror/",
    resolvers += "trafigura" at "http://nexus.global.trafigura.com:8081/nexus/content/repositories/tooling-releases/",
    organizationName := "Trafigura",
    version := starlingVersion
  )

  lazy val standardSettingsNexus = Defaults.defaultSettings ++ Seq(
    unmanagedSourceDirectories in Compile <+= baseDirectory(_/"src"),
    unmanagedSourceDirectories in Test <+= baseDirectory(_/"tests"),
    unmanagedResourceDirectories in Test <+= baseDirectory(_/"test-resources"),
    unmanagedResourceDirectories in Compile <+= baseDirectory(_/"resources"),
    unmanagedClasspath in Test <+= (baseDirectory) map { bd => Attributed.blank(bd / "resources") },
    ivyXML := <dependencies><exclude artifact="jcl-over-slf4j"/><exclude artifact="junit"/></dependencies>,
    scalaVersion := "2.9.1",
    showLibsTask,
    writeClasspathScriptTask,
    credentialsSetting,
    publishSetting,
    resolvers += "Non-Trafigura Public Repositories" at "http://nexus.global.trafigura.com:8081/nexus/content/groups/mirror/",
    resolvers += "trafigura" at "http://nexus.global.trafigura.com:8081/nexus/content/repositories/tooling-releases/",
    resolvers += "Titan Cross Stream Snapshots" at "http://nexus.global.trafigura.com:8081/nexus/content/repositories/titan-cross-stream-snapshots/",
    organizationName := "Trafigura",
    version := starlingVersion
  )

  lazy val publishSetting = publishTo <<= (version) {
    version: String =>
      def repo(name: String) = {
        if (starlingVersion.trim.toLowerCase == "snapshot") {
          name at "http://nexus.global.trafigura.com:8081/nexus/content/repositories/starling-test/" + name
        } else {
          name at "http://nexus.global.trafigura.com:8081/nexus/content/repositories/starling-releases/" + name
        }
      }
//      val isSnapshot = version.trim.endsWith("SNAPSHOT")
//      val repoName   = if(isSnapshot) "snapshots" else "releases"
//      Some(repo(repoName))
     Some(repo("starling-releases"))
  }

  lazy val credentialsSetting = credentials += {
    /*Seq("admin", "admin123").map(k => Option(System.getProperty(k))) match {
      case Seq(Some(user), Some(pass)) =>
        Credentials("Sonatype Nexus Repository Manager", "nexus-direct.scala-tools.org", user, pass)
      case _ =>
        Credentials(Path.userHome / ".ivy2" / ".credentials")
    }*/
    Credentials("Sonatype Nexus Repository Manager", "nexus.global.trafigura.com", "admin", "admin123")
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
    "cglib" % "cglib-nodep" % "2.2" withSources(),
    "joda-time" % "joda-time" % "1.6" withSources(),
    "com.rabbitmq" % "amqp-client" % "1.7.2" withSources(),
    "log4j" % "log4j" % "1.2.16" withSources(),
    "org.slf4j" % "slf4j-log4j12" % "1.6.1" withSources(),
    "com.google.collections" % "google-collections" % "1.0" withSources(),
    "commons-codec" % "commons-codec" % "1.4" withSources(),
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
  ) dependsOn(daterange, quantity)

  val mathsDependencies = Seq(
    "org.apache.commons" % "commons-math" % "2.1"
  )

  lazy val maths = Project(
    "maths",
    file("./maths"),
    settings = standardSettingsNexus ++ (libraryDependencies ++= mathsDependencies)
  ) dependsOn(quantity % testDependency, daterange % testDependency)


  val titanModelDependencies = Seq(
    "org.slf4j" % "slf4j-api" % "1.6.1",
    "dom4j" % "dom4j" % "1.6.1",
    "com.rabbitmq" % "amqp-client" % "1.7.2",
    "joda-time" % "joda-time" % "1.6",
    "org.codehaus.jettison" % "jettison" % "1.1",
    "commons-httpclient" % "commons-httpclient" % "3.1",
    "com.trafigura.tradinghub" % "scala-hub-support" % "2.17",
    "com.trafigura.tradinghub" % "persistence-support" % "2.17",
    "org.jboss.resteasy" % "jaxrs-api" % "1.2.GA"
  )

  import TitanModel._
  lazy val titanModel = Project(
    "titan-model", 
    modelRoot,
    settings = standardSettingsNexus ++ Seq(
      unmanagedSourceDirectories in Compile <+= baseDirectory(_/"model-src"),
      cleanGenSrcTask := cleanGenSrc, 
      cleanCopiedSrcTask := cleanCopiedSrc, 
      clean <<= clean.dependsOn(cleanGenSrcTask, cleanCopiedSrcTask),
      buildSrcTask := buildSource,
      compile in Compile <<= (compile in Compile).dependsOn(buildSrcTask),
      libraryDependencies ++= titanModelDependencies
    ) 
    ++ copyModelSettings
  )

  lazy val starlingApi = if (useTitanModelBinaries) {
    Project(
      "starling-api", 
      file("./starling.api"),
      settings = standardSettingsNexus ++
        Seq(unmanagedJars in Compile <++= (baseDirectory) map titanBinaryJars) ++ 
        Seq(unmanagedJars in Runtime <++= (baseDirectory) map titanBinaryJars) ++ 
        Seq(unmanagedJars in Test <++= (baseDirectory) map titanBinaryJars)
    ) dependsOn(titanReturnTypes)
  } else {
    Project(
      "starling-api", 
      file("./starling.api"),
      settings = standardSettingsNexus
    ) dependsOn(titanModel, titanReturnTypes)
  }

  lazy val props = Project(
    "props",
    file("./props"),
    settings = standardSettingsNexus
  ) dependsOn(utils, manager)

  val authDependencies = Seq(
//    "net.java.dev.jna" % "jna" % "3.3.0", Put this back in once sbt can handle classifiers properly
    "sbt/bug/net/java/dev/jna" % "jna" % "3.3.0",
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
    "org.swinglabs" % "swingx-core" % "1.6.2-2",
    "mig" % "miglayout" % "4.0" classifier "swing",
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
    settings = standardSettingsNexus
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

  lazy val titan = if (useTitanModelBinaries){
		Project(
				"titan", 
				file("./titan"),
					settings = standardSettingsNexus ++
						Seq(unmanagedJars in Compile <++= (baseDirectory) map titanBinaryJars) ++ 
						Seq(unmanagedJars in Runtime <++= (baseDirectory) map titanBinaryJars) ++ 
						Seq(unmanagedJars in Test <++= (baseDirectory) map titanBinaryJars)
			) dependsOn(curves % "test->test", databases)
	} else {
		Project(
				"titan", 
				file("./titan"),
				settings = standardSettingsNexus
			) dependsOn(curves % "test->test", titanModel, databases)
	}

  def titanBinaryJars(base : File) : Seq[Attributed[File]] = (((base / "../lib/titan-model-jars") ** "*.jar")).getFiles.map{f : File => Attributed.blank(f)}
  
  val servicesDependencies = Seq(
    "net.liftweb" % "lift-json_2.9.0" % "2.4-M2",
    "javax.mail" % "mail" % "1.4",
    "org.mortbay.jetty" % "jetty" % "6.1.26",
    "org.mortbay.jetty" % "jetty-util" % "6.1.26",
    "org.subethamail" % "subethasmtp-wiser" % "1.2",
    "org.subethamail" % "subethasmtp-smtp" % "1.2",
    "org.springframework" % "spring-context-support" % "3.0.5.RELEASE",
    "com.thoughtworks.paranamer" % "paranamer" % "2.3",
    "starling-external-jars" % "xlloop" % "0.3.1",
    "commons-httpclient" % "commons-httpclient" % "3.1",
    "org.jboss.resteasy" % "jaxrs-api" % "1.2.GA",
    "org.jboss.resteasy" % "resteasy-jaxrs" % "2.2.2.GA",
    "org.scannotation" % "scannotation" % "1.0.2",
    "javax.servlet" % "servlet-api" % "2.5",
  
    "com.trafigura.titan.shared-libs" % "titan-core" % "1.0-SNAPSHOT" notTransitive(),
    "com.trafigura.titan.shared-libs" % "titan-security" % "1.0-SNAPSHOT" notTransitive(),
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
  ) dependsOn(startserver, gui, singleClasspathManager)

  val webserviceDependencies = Seq(
    "javax.servlet" % "servlet-api" % "2.5",
    "org.jboss.resteasy" % "jaxrs-api" % "1.2.GA",
    "net.liftweb" % "lift-json_2.9.0" % "2.4-M2",
    "org.mortbay.jetty" % "jetty" % "6.1.26",
    "org.mortbay.jetty" % "jetty-util" % "6.1.26",
    "com.thoughtworks.paranamer" % "paranamer" % "2.3",
    "org.jboss.resteasy" % "resteasy-jaxrs" % "2.2.2.GA",
    "org.scannotation" % "scannotation" % "1.0.2"
  )

  lazy val webservice = Project(
    "webservice",
    file("./webservice"),
    settings = standardSettingsNexus ++ (libraryDependencies ++= webserviceDependencies)
  ) dependsOn(utils, manager, props, daterange, starlingApi)

  // Evil hack so that I can get a classpath exported including the test-classes of all projects.
  // See bin/write-classpath-script.sh
  lazy val dummy = if (useTitanModelBinaries) {
    Project(
      "dummy",
      file("./dummy-sbt-vim-hack"),
      settings = standardSettingsNexus ++
        Seq(unmanagedClasspath in Compile <++= (baseDirectory) map titanBinaryJars) ++ 
        Seq(unmanagedClasspath in Test <++= (baseDirectory) map titanBinaryJars)
    ) dependsOn(
      childProjects.map(_ % "test->test") : _*
    )
  } else {
    Project(
      "dummy",
      file("./dummy-sbt-vim-hack"),
      settings = standardSettingsNexus
    ) dependsOn(
      childProjects.map(_ % "test->test") : _*
    )
  }

  def titanModelReference : List[ProjectReference] = if (useTitanModelBinaries) Nil else List(titanModel) 
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

  def childProjects : List[ProjectReference] =  otherProjectRefereneces ::: titanModelReference

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
    titanModel, 
    tradeFacility)

  val sharedProjects : List[ProjectReference] =  List(
      utils, quantity, daterange, titanReturnTypes, titan, titanModel, databases, starlingApi)
  
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

  object TitanModel {
    import IO._
    val modelGenSrcDir = file("titan-scala-model/model-src/main/scala/")
    val copiedSrcDir = file("titan-scala-model/src")
    val modelRoot = file("titan-scala-model")
    def cleanGenSrc = IO.delete(modelGenSrcDir)
    def cleanCopiedSrc = IO.delete(copiedSrcDir) 
    val cleanGenSrcTask = TaskKey[Unit]("clean-src", "Clean model generated sources")
    val cleanCopiedSrcTask = TaskKey[Unit]("clean-copied-src", "Clean sources copied from model")
    val buildSrcTask = TaskKey[Unit]("build-src", "Build sources from model")
     
    val copyModelJarForIdea = TaskKey[Unit]("copy-model", "copy the edm model to the stored location in Git that is referenced by IntelliJ IDEA")

    lazy val copyModelSettings : Seq[sbt.Project.Setting[_]] = Seq(
      copyModelJarForIdea <<= (packageBin in Compile) map(_ => copyModelJar)
    )

    def copyModelJar {
      val srcFile = new File(modelRoot + "/target/scala-2.9.1/titan-model_2.9.1-0.1.jar")
      val destFile = new File("./lib/titan-model-jars/scala-model-with-persistence.jar")
      println("copying target jar %s to %s".format(srcFile, destFile))
      val r = copyFile(srcFile, destFile)
      println("copied model jar")
      r
    }

    def buildSource {
      lazy val buildUsingBinaryTooling = true
      lazy val rubyModelPathFinder = {
        (new File(modelRoot, "/../../../model/model/")** "*.rb")
      }
      
      def latestRubyFileTime = {
        val files = rubyModelPathFinder.getFiles
        if (files.isEmpty)
          throw new Exception("No ruby files found")
        files.map(_.lastModified).toList.sort(_>_).head
      }
      
      def earliestScalaFileTime = {
        (modelGenSrcDir ** "*.scala").getFiles.toList.map(_.lastModified).sort(_<_) match {
          case Nil => None
          case t :: _ => Some(t)
        }
      }

      val toolingLauncher = if (buildUsingBinaryTooling == true) new File(modelRoot, "../../../mdl/bindinggen.rb") else new File(modelRoot, "/model/tooling/binding-generator/thubc.rb")

      val generateModelMainSourceCmd = new java.lang.ProcessBuilder("ruby", toolingLauncher.getAbsolutePath, "-o", modelGenSrcDir.getAbsolutePath, "-b", "../../../mdl/starling/bindings.rb", "../../../mdl/starling/model.rb") directory modelRoot

      lazy val nonModelSourcePath = new File(modelRoot, "src")
      def copyNonModelSource  = {
        if (! (nonModelSourcePath.exists)) {
          val originalSourcePath = new File(modelRoot, "../../../model/model/scala-model-with-persistence/src/")
          copyDirectory(originalSourcePath, nonModelSourcePath)
          val hibernateBean = new File (modelRoot, "/src/main/scala/com/trafigura/refinedmetals/persistence/CustomAnnotationSessionFactoryBean.scala")
          //println("***** DEBUG ***** path " + hibernateBean.getAbsolutePath + ", " + hibernateBean.exists + ", " + hibernateBean.canWrite) 
          if (hibernateBean.exists && hibernateBean.canWrite) hibernateBean.delete()
        }
        None
      }

      (latestRubyFileTime, earliestScalaFileTime) match {
        case (t_ruby, Some(t_scala)) if t_ruby < t_scala => 
        case _ => copyNonModelSource; generateModelMainSourceCmd !; 
      }      
    }
  }

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
  }
}

