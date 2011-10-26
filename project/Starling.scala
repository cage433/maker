import sbt._
import Keys._
import java.io.File


object StarlingBuild extends Build{

  import Utils._

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
    writeClasspathScriptTask
  )

  val testDependency = "compile;test->test"

  lazy val manager = Project(
    "manager",
    file("./manager"),
    settings = standardSettings 
  ) 

  lazy val utils = Project(
    "utils", 
    file("./utils"), 
    settings = standardSettings 
  )

  lazy val osgiRun = Project(
    "osgirun",
    file("./osgirun"),
    settings = standardSettings
  ) 

  lazy val concurrent = Project(
    "concurrent", 
    file("./concurrent"),
    settings = standardSettings
  ) dependsOn (utils) 

  lazy val quantity = Project(
    "quantity", 
    file("./quantity"),
    settings = standardSettings
  ) dependsOn (utils)

  lazy val osgiManager = Project(
    "osgimanager",
    file("./osgimanager"),
    settings = standardSettings 
  ) dependsOn(manager, utils)

  lazy val singleClasspathManager = Project(
    "singleclasspathmanager",
    file("./singleclasspathmanager"),
    settings = standardSettings
  ) dependsOn(manager, utils, osgiManager)

  lazy val pivot = Project(
    "pivot", 
    file("./pivot"),
    settings = standardSettings
  ) dependsOn(quantity)

  lazy val pivotUtils = Project(
    "pivot.utils", 
    file("./pivot.utils"),
    settings = standardSettings
  ) dependsOn(daterange, pivot)

  lazy val daterange = Project(
    "daterange", 
    file("./daterange"),
    settings = standardSettings
  ) dependsOn(utils)


  lazy val titanReturnTypes = Project(
    "titan-return-types",
    file("./titan.return.types"),
    settings = standardSettings
  ) dependsOn(daterange, quantity)

  lazy val maths = Project(
    "maths", 
    file("./maths"),
    settings = standardSettings
  ) dependsOn(quantity % testDependency, daterange % testDependency)


  import TitanModel._
  lazy val titanModel = Project(
    "titan-model", 
    modelRoot,
    settings = standardSettings ++ Seq(
      unmanagedSourceDirectories in Compile <+= baseDirectory(_/"model-src"),
      cleanGenSrcTask := cleanGenSrc, 
      cleanCopiedSrcTask := cleanCopiedSrc, 
      clean <<= clean.dependsOn(cleanGenSrcTask, cleanCopiedSrcTask),
      buildSrcTask := buildSource,
      compile in Compile <<= (compile in Compile).dependsOn(buildSrcTask)
    ) 
    ++ copyModelSettings
  )

  lazy val starlingApi = if (useTitanModelBinaries) {
    Project(
      "starling-api", 
      file("./starling.api"),
      settings = standardSettings ++ 
        Seq(unmanagedJars in Compile <++= (baseDirectory) map titanBinaryJars) ++ 
        Seq(unmanagedJars in Runtime <++= (baseDirectory) map titanBinaryJars) ++ 
        Seq(unmanagedJars in Test <++= (baseDirectory) map titanBinaryJars)
    ) dependsOn(titanReturnTypes)
  } else {
    Project(
      "starling-api", 
      file("./starling.api"),
      settings = standardSettings 
    ) dependsOn(titanModel, titanReturnTypes)
  }

  lazy val props = Project(
    "props",
    file("./props"),
    settings = standardSettings
  ) dependsOn(utils, manager)

  lazy val auth = Project(
    "auth", 
    file("./auth"),
    settings = standardSettings
  ) dependsOn (utils, manager, props)

  lazy val bouncyrmi = Project(
    "bouncyrmi", 
    file("./bouncyrmi"),
    settings = standardSettings
  ) dependsOn(manager, auth, props)

  lazy val loopyxl = Project(
    "loopyxl", 
    file("./loopyxl"),
    settings = standardSettings
  ) dependsOn(manager, auth)

  lazy val browserService = Project(
    "browser-service",
    file("./browser.service"),
    settings = standardSettings
  ) dependsOn(manager)

  lazy val browser = Project(
    "browser",
    file("./browser"),
    settings = standardSettings
  ) dependsOn(browserService, manager)

  lazy val guiapi = Project(
    "gui-api", 
    file("./gui.api"),
    settings = standardSettings
  ) dependsOn(pivotUtils, quantity, auth, bouncyrmi, browserService, manager)

  lazy val fc2Facility = Project(
    "fc2-facility",
    file("./fc2.facility"),
    settings = standardSettings
  ) dependsOn(daterange, guiapi)

  lazy val curves = Project(
    "curves", 
    file("./curves"),
    settings = standardSettings
  ) dependsOn(utils % testDependency, daterange % testDependency, maths, pivotUtils, guiapi, quantity % testDependency)

  lazy val instrument = Project(
    "instrument", 
    file("./instrument"),
    settings = standardSettings
  ) dependsOn(curves % testDependency, daterange % testDependency, titanReturnTypes)

  lazy val reportsFacility = Project(
    "reports-facility",
    file("./reports.facility"),
    settings = standardSettings
  ) dependsOn(guiapi)

  lazy val rabbitEventViewerApi = Project(
    "rabbit-event-viewer-api",
    file("./rabbit.event.viewer.api"),
    settings = standardSettings
  ) dependsOn(pivot, manager)

  lazy val gui = Project(
    "gui", 
    file("./gui"),
    settings = standardSettings ++ libJars("servlet-api-jar-2.5.jar")
  ) dependsOn(fc2Facility, tradeFacility, reportsFacility, browser, rabbitEventViewerApi, singleClasspathManager)

  lazy val tradeFacility = Project(
    "trade", 
    file("./trade.facility"),
    settings = standardSettings 
  ) dependsOn(auth, guiapi, manager)

 
  lazy val starlingClient = Project(
    "starling-client",
    file("./starling.client"),
    settings = standardSettings
  ) dependsOn(starlingApi, bouncyrmi)

  lazy val dbx = Project(
    "dbx",
    file("./dbx"),
    settings = standardSettings
  ) dependsOn(manager, utils, instrument)

  lazy val databases = Project(
    "databases", 
    file("./databases"),
    settings = standardSettings 
  ) dependsOn(curves % "test->test", pivot , guiapi , concurrent , auth , starlingApi, dbx, props)

  lazy val rabbitEventViewerService = Project(
    "rabbit-event-viewer-service",
    file("./rabbit.event.viewer.service"),
    settings = standardSettings
  ) dependsOn(rabbitEventViewerApi, pivot, databases, manager)

  lazy val titan = if (useTitanModelBinaries){
		Project(
				"titan", 
				file("./titan"),
					settings = standardSettings ++ 
						Seq(unmanagedJars in Compile <++= (baseDirectory) map titanBinaryJars) ++ 
						Seq(unmanagedJars in Runtime <++= (baseDirectory) map titanBinaryJars) ++ 
						Seq(unmanagedJars in Test <++= (baseDirectory) map titanBinaryJars)
			) dependsOn(curves % "test->test", databases)
	} else {
		Project(
				"titan", 
				file("./titan"),
				settings = standardSettings 
			) dependsOn(curves % "test->test", titanModel, databases)
	}

  def titanBinaryJars(base : File) : Seq[Attributed[File]] = (((base / "../lib/titan-model-jars") ** "*.jar")).getFiles.map{f : File => Attributed.blank(f)}
  def libJars(jarNames: String*): Seq[Setting[_]] = {
    def libJar(jarName: String): Seq[Setting[_]] = {
      def jars(base : File) : Seq[Attributed[File]] = (((base / "../lib/") ** jarName)).getFiles.map{f : File => Attributed.blank(f)}

      Seq(unmanagedJars in Compile <++= (baseDirectory) map jars) ++
      Seq(unmanagedJars in Runtime <++= (baseDirectory) map jars) ++
      Seq(unmanagedJars in Test <++= (baseDirectory) map jars)
    }

    jarNames.flatMap(libJar(_))
  }

  lazy val services = Project(
    "services", 
    file("./services"),
    settings = standardSettings 
  ) dependsOn(curves % "test->test", concurrent, loopyxl, titan, gui, fc2Facility, browser, titanReturnTypes)

  lazy val tradeImpl = Project(
    "trade-impl",
    file("./trade.impl"),
    settings = standardSettings
  ) dependsOn(services, tradeFacility, manager)

  lazy val metals = Project(
    "metals",
    file("./metals"),
    settings = standardSettings
  ) dependsOn(services, tradeImpl)

  lazy val reportsImpl = Project(
    "reports-impl",
    file("./reports.impl"),
    settings = standardSettings
  ) dependsOn(services)


  lazy val startserver = Project(
    "startserver",
    file("./startserver"),
    settings = standardSettings
  ) dependsOn(services, reportsImpl, tradeImpl, metals, starlingClient, singleClasspathManager, rabbitEventViewerService, webservice)

  lazy val launcher = Project(
    "launcher", 
    file("./launcher"),
    settings = standardSettings
  ) dependsOn(startserver, gui, singleClasspathManager)

  lazy val webservice = Project(
    "webservice",
    file("./webservice"),
    settings = standardSettings ++ libJars("servlet-api-jar-2.5.jar", "jaxrs-api-1.2.GA.jar", "lift-json_2.9.0-jar-2.4-M2.jar")
  ) dependsOn(utils, manager, props, daterange, starlingApi)

  // Evil hack so that I can get a classpath exported including the test-classes of all projects.
  // See bin/write-classpath-script.sh
  lazy val dummy = if (useTitanModelBinaries) {
    Project(
      "dummy",
      file("./dummy-sbt-vim-hack"),
      settings = standardSettings ++
        Seq(unmanagedClasspath in Compile <++= (baseDirectory) map titanBinaryJars) ++ 
        Seq(unmanagedClasspath in Test <++= (baseDirectory) map titanBinaryJars)
    ) dependsOn(
      childProjects.map(_ % "test->test") : _*
    )
  } else {
    Project(
      "dummy",
      file("./dummy-sbt-vim-hack"),
      settings = standardSettings
    ) dependsOn(
      childProjects.map(_ % "test->test") : _*
    )
  }

  def titanModelReference : List[ProjectReference] = if (useTitanModelBinaries) Nil else List(titanModel) 
  def otherProjectRefereneces : List[ProjectReference] = List(
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
    webservice
  )

  val childProjects : List[ProjectReference] =  otherProjectRefereneces ::: titanModelReference

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
  
  val docSharedRoot = Project("doc-shared", file("doc.shared"), settings = standardSettings ++ Seq(
      allSources <<= sharedProjects.map(sources in Compile in _).join, // join: Seq[Task[A]] => Task[Seq[A]]
      //allSourceDirectories <<= childProjects.map(sourceDirectories in Compile in _).join,
      //allPackagedArtifacts <<= childProjects.map(packagedArtifacts in _).join,

      // Combine the sources of other modules to generate Scaladoc and SXR annotated sources
      (sources in Compile) <<= (allSources).map(_.flatten),
      (unmanagedJars in Compile) <<= ((childProjects.map(unmanagedJars in Compile in _).join).map(_.flatten)),

      // Avoid compiling the sources here; we just are after scaladoc.
      (compile in Compile) := inc.Analysis.Empty)
    )
  

  val docAllRoot = Project("doc-all", file("doc.all"), settings = standardSettings ++ Seq(
      allSources <<= docProjects.map(sources in Compile in _).join, // join: Seq[Task[A]] => Task[Seq[A]]
      //allSourceDirectories <<= childProjects.map(sourceDirectories in Compile in _).join,
      //allPackagedArtifacts <<= childProjects.map(packagedArtifacts in _).join,

      // Combine the sources of other modules to generate Scaladoc and SXR annotated sources
      (sources in Compile) <<= (allSources).map(_.flatten),
      (unmanagedJars in Compile) <<= ((childProjects.map(unmanagedJars in Compile in _).join).map(_.flatten)),

      // Avoid compiling the sources here; we just are after scaladoc.
      (compile in Compile) := inc.Analysis.Empty)
    )

  val root = Project("starling", file("."), settings = standardSettings) aggregate (childProjects : _*)

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
      val resourceDirs = cp.map(_.data).getFiles.toList.map(_.getPath).filter(_.endsWith("/classes")).map{s => s.replace("/classes", "/resources")}
      file.println("export CLASSPATH=" + (cp.map(_.data).getFiles.toList ::: resourceDirs).mkString(":"))
      file.println("export JAVA_OPTS='-server -XX:MaxPermSize=1024m -Xss512k -Xmx6000m'")
      file.close()
      None
    }
  }
}

