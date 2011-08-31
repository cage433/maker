import sbt._
import Keys._
import java.io.File

object Dependencies{

  val testDependencies = Seq(
    "org.mockito" % "mockito-all" % "1.8.2" withSources(),
    "org.testng" % "testng" % "5.8" classifier "jdk15" withSources()
  ) 

  val utilsDependencies = Seq(
    "cglib" % "cglib-nodep" % "2.2" withSources(),
    "joda-time" % "joda-time" % "1.6" withSources(),
    "com.rabbitmq" % "amqp-client" % "1.7.2" withSources(),
    "log4j" % "log4j" % "1.2.16" withSources(),
    "org.slf4j" % "slf4j-log4j12" % "1.6.1" withSources(),
    "com.google.collections" % "google-collections" % "1.0" withSources(),
    "commons-codec" % "commons-codec" % "1.4" withSources(),
    "colt" % "colt" % "1.0.3",
    "com.thoughtworks.xstream" % "xstream" % "1.3.1" withSources(),
    "org.scala-tools" %% "scala-stm" % "0.3",
    "org.testng" % "testng" % "5.8" classifier "jdk15" withSources()
  ) ++ testDependencies

  val bouncyRmiDependencies = Seq(
    "cglib" % "cglib-nodep" % "2.2" withSources(),
    "org.jboss.netty" % "netty" % "3.2.5.Final" withSources(),
    "commons-io" % "commons-io" % "1.3.2" withSources(),
    "org.scala-lang" % "scala-swing" % "2.9.0-1" withSources()
  )

  val browserServiceDependencies = Seq(
    "org.scala-lang" % "scala-swing" % "2.9.0-1" withSources()
  )

  val authDependencies = Seq("com.sun.jna" % "jna" % "3.0.9" withSources())

  val loopyxlDependencies = Seq(
    "com.google.protobuf" % "protobuf-java" % "2.3.0" withSources()
  ) 

  val guiApiDependencies = Seq( 
    "net.debasishg" % "sjson_2.8.0" % "0.8" intransitive() withSources()
  ) 
  val guiDependencies = Seq(
    "jfree" % "jfreechart" % "1.0.0"
  ) 

  val browserDependencies = Seq(
    "org.scala-lang" % "scala-swing" % "2.9.0-1" withSources(),
    "com.thoughtworks.xstream" % "xstream" % "1.3.1" withSources(),
    "com.google.collections" % "google-collections" % "1.0" withSources()
  )

  val databasesDependencies = Seq(
    "org.springframework" % "spring-jdbc" % "3.0.5.RELEASE" withSources(),
    "com.jolbox" % "bonecp" % "0.7.1.RELEASE" intransitive() withSources(),
    "org.slf4j" % "slf4j-api" % "1.6.1" withSources(),
    "org.scala-tools.testing" % "scalacheck_2.9.0-1" % "1.9" withSources(),
    "org.apache.derby" % "derby" % "10.5.3.0_1",
    "hsqldb" % "hsqldb" % "1.8.0.10" % "test",
    "com.h2database" % "h2" % "1.2.131" % "test" withSources()
  ) 
  
  val titanModelDependencies = Seq(
    "org.slf4j" % "slf4j-api" % "1.6.1" withSources(),
    "dom4j" % "dom4j" % "1.6.1" withSources(),
    "com.rabbitmq" % "amqp-client" % "1.7.2" withSources(),
    "joda-time" % "joda-time" % "1.6" withSources(),
    "org.codehaus.jettison" % "jettison" % "1.1" withSources(),
    "commons-httpclient" % "commons-httpclient" % "3.1",

      "com.trafigura.tradinghub" % "scala-hub-support" % "2.14",
      "com.trafigura.tradinghub" % "persistence-support" % "2.14"
  )

  val servicesDependencies = Seq(
    "net.liftweb" % "lift-json_2.9.0" % "2.4-M2" withSources(),
    "javax.mail" % "mail" % "1.4" withSources(),
    "javax.servlet" % "servlet-api" % "2.5" withSources(),
    "org.mortbay.jetty" % "jetty" % "6.1.26" withSources(),
    "org.subethamail" % "subethasmtp-wiser" % "1.2" % "test" withSources(),
    "org.subethamail" % "subethasmtp-smtp" % "1.2" % "test" withSources(),
    "org.springframework" % "spring-context-support" % "3.0.5.RELEASE" withSources()
  ) 

  val titanSharedDependencies = Seq(
    "com.trafigura.services" % "titan-core" % "LATEST",
    "com.trafigura.services" % "titan-security" % "LATEST",
    "com.trafigura.services" % "titan-utils" % "LATEST"
  )
}

object StarlingBuild extends Build{

  import Dependencies._
  import Utils._

  val useTitanModelBinaries = {
    if (!new File("props.conf").exists)
      true
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
  lazy val standardSettings = Defaults.defaultSettings ++ Seq(
    unmanagedSourceDirectories in Compile <+= baseDirectory(_/"src"),
    unmanagedSourceDirectories in Test <+= baseDirectory(_/"tests"),
    unmanagedResourceDirectories in Test <+= baseDirectory(_/"test-resources"),
    unmanagedResourceDirectories in Compile <+= baseDirectory(_/"resources"),
    unmanagedBase <<= baseDirectory( (base: File) => base /"lib"),
    unmanagedClasspath in Test <+= (baseDirectory) map { bd => Attributed.blank(bd / "resources") },
    ivyXML := <dependencies><exclude artifact="jcl-over-slf4j"/><exclude artifact="junit"/></dependencies>, 
    scalaVersion := "2.9.0-1",
    showLibsTask,
    cleanKeepFiles <+= baseDirectory(_/"target/scala-2.9.0.1/cache"),
    writeClasspathScriptTask
  )

  val testDependency = "compile;test->test"

  val crazyTestListener = new sbt.TestsListener{
    //def contentLogger (test: TestDefinition) : Option[ContentLogger] = None                                                                                                                                        

    //Used by the test framework for logging test results
    def doComplete (finalResult: TestResult.Value){
      println("Called doComplete")
    }

    //called once, at end.
    def doInit{
      println("Called doInit")
    }

    //called once, at beginning.
    def endGroup (name: String, result: TestResult.Value) {
      println("Called endGroup")
    }

    //called if test completed
    def endGroup (name: String, t: Throwable){
      println("Called endGroup")
    }

    //called if there was an error during test
    def startGroup (name: String){
      println("Called startGroup")
    }

    //called for each class or equivalent grouping
    def testEvent (event: TestEvent){
      println("Called testEvent " + event)
    }
  }

  lazy val utils = Project(
    "utils", 
    file("./utils"), 
    //settings = standardSettings ++ Seq(libraryDependencies ++= utilsDependencies, testListeners += crazyTestListener)
    settings = standardSettings ++ Seq(libraryDependencies ++= utilsDependencies)
  )

  lazy val bouncyrmi = Project(
    "bouncyrmi", 
    file("./bouncyrmi"),
    settings = standardSettings ++ Seq(libraryDependencies ++= bouncyRmiDependencies)
  )
  lazy val auth = Project(
    "auth", 
    file("./auth"),
    settings = standardSettings ++ Seq(libraryDependencies ++= authDependencies)
  ) dependsOn (utils, bouncyrmi)

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

  lazy val daterange = Project(
    "daterange", 
    file("./daterange"),
    settings = standardSettings
  ) dependsOn(utils)

  lazy val loopyxl = Project(
    "loopyxl", 
    file("./loopyxl"),
    settings = standardSettings ++ Seq(libraryDependencies ++= loopyxlDependencies)
  ) dependsOn(bouncyrmi, auth)

  lazy val maths = Project(
    "maths", 
    file("./maths"),
    settings = standardSettings
  ) dependsOn(quantity % testDependency, daterange % testDependency)

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

  lazy val guiapi = Project(
    "gui.api", 
    file("./gui.api"),
    settings = standardSettings ++ Seq(libraryDependencies ++= guiApiDependencies)
  ) dependsOn(pivotUtils, quantity, auth, bouncyrmi, browserService)

  lazy val fc2api = Project(
    "fc2.api",
    file("./fc2.api"),
    settings = standardSettings ++ Seq(libraryDependencies ++= guiApiDependencies)
  ) dependsOn(daterange, guiapi)

  lazy val curves = Project(
    "curves", 
    file("./curves"),
    settings = standardSettings ++ Seq(libraryDependencies ++= testDependencies)
  ) dependsOn(utils % "compile;test->test", daterange % "test->test", maths, pivotUtils, guiapi)

  lazy val instrument = Project(
    "instrument", 
    file("./instrument"),
    settings = standardSettings ++ Seq(libraryDependencies ++= testDependencies) 
  ) dependsOn(curves % "compile;test->test", daterange % "test->test")

  lazy val gui = Project(
    "gui", 
    file("./gui"),
    settings = standardSettings ++ Seq(libraryDependencies ++= guiDependencies)
  ) dependsOn(guiapi, fc2api, browser)

  lazy val browser = Project(
    "browser",
    file("./browser"),
    settings = standardSettings ++ Seq(libraryDependencies ++= browserDependencies)
  ) dependsOn(browserService)

  lazy val browserService = Project(
    "browser.service",
    file("./browser.service"),
    settings = standardSettings ++ Seq(libraryDependencies ++= browserServiceDependencies)
  ) dependsOn()

  lazy val trade = Project(
    "trade", 
    file("./trade"),
    settings = standardSettings 
  ) dependsOn(instrument)

  lazy val VaR = Project(
    "var", 
    file("./var"),
    settings = standardSettings 
  ) dependsOn(trade)


  import TitanModel._
  lazy val titanModel = Project(
    "titan-model", 
    modelRoot,
    settings = standardSettings ++ Seq(
      libraryDependencies ++= titanModelDependencies,
      resolvers ++= Seq(
        "Trafigura Nexus Repository" at "http://nexus.global.trafigura.com:8081/nexus/content/repositories/tooling-releases/",
        "Alfresco (needed for resteasy 1.2)" at "http://maven.alfresco.com/nexus/content/groups/public/"
      ),
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
      "starlingApi", 
      file("./starling.api"),
      settings = standardSettings ++ 
        Seq(unmanagedClasspath in Compile <++= (baseDirectory) map titanBinaryJars) ++ 
        Seq(unmanagedClasspath in Test <++= (baseDirectory) map titanBinaryJars)
    ) dependsOn(bouncyrmi)
  } else {
    Project(
      "starlingApi", 
      file("./starling.api"),
      settings = standardSettings 
    ) dependsOn(bouncyrmi, titanModel)
  }
 
  lazy val databases = Project(
    "databases", 
    file("./databases"),
    settings = standardSettings ++ Seq(libraryDependencies ++= databasesDependencies ++ testDependencies)
  ) dependsOn(curves % "test->test", VaR , pivot , guiapi , concurrent , auth , starlingApi )

  lazy val titan = if (useTitanModelBinaries) {
    Project(
      "titan", 
      file("./titan"),
      settings = standardSettings ++ 
        Seq(libraryDependencies ++= testDependencies) ++ 
        Seq(unmanagedClasspath in Compile <++= (baseDirectory) map titanBinaryJars) ++ 
        Seq(unmanagedClasspath in Test <++= (baseDirectory) map titanBinaryJars)
    ) dependsOn(curves % "test->test", databases)
  }
  else {
    Project(
      "titan", 
      file("./titan"),
      settings = standardSettings ++ Seq(libraryDependencies ++= testDependencies) 
    ) dependsOn(curves % "test->test", titanModel, databases)
  }

  def titanBinaryJars(base : File) : Seq[Attributed[File]] = (((base / "../lib/titan-model-jars") ** "*.jar")).getFiles.map{f : File => Attributed.blank(f)}

  lazy val services = if (useTitanModelBinaries) {
    Project(
      "services", 
      file("./services"),
      settings = standardSettings ++ 
        Seq(libraryDependencies ++= servicesDependencies ++ testDependencies) ++ 
        Seq(unmanagedClasspath in Compile <++= (baseDirectory) map titanBinaryJars) ++ 
        Seq(unmanagedClasspath in Test <++= (baseDirectory) map titanBinaryJars)
    ) dependsOn(curves % "test->test", loopyxl % "test->test", bouncyrmi, concurrent, loopyxl, titan, gui, fc2api, browser)
  } else {
    Project(
      "services", 
      file("./services"),
      settings = standardSettings ++ Seq(
        libraryDependencies ++= servicesDependencies ++ testDependencies
      )
    ) dependsOn(curves % "test->test", loopyxl % "test->test", bouncyrmi, concurrent, loopyxl, titan, gui, fc2api, browser)
  }

  lazy val devLauncher = Project(
    "devLauncher", 
    file("./dev.launcher"),
    settings = standardSettings
  ) dependsOn(services, gui)

  // Evil hack so that I can get a classpath exported including the test-classes of all projects.
  // See bin/write-classpath-script.sh
  lazy val dummy = Project(
    "dummy",
    file("./dummy-sbt-vim-hack"),
    settings = standardSettings
  ) dependsOn(
    childProjects.map(_ % "test->test") : _*
  )

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
    fc2api,
    curves,
    instrument,
    gui,
    browser,
    browserService,
    trade,
    VaR,
    databases,
    titan,
    services,
    devLauncher
  )

  val childProjects : List[ProjectReference] =  otherProjectRefereneces ::: titanModelReference


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
      val srcFile = new File(modelRoot + "/target/scala-2.9.0.1/titan-model_2.9.0-1-0.1.jar")
      val destFile = new File("./lib/titan-model-jars/scala-model-with-persistence.jar")
      println("copying target jar %s to %s".format(srcFile, destFile))
      val r = copyFile(srcFile, destFile)
      println("copied model jar")
      r
    }

    def buildSource {
      lazy val buildUsingBinaryTooling = true
      
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

      lazy val rubyModelPathFinder = {
        (new File(modelRoot, "/../../../model/model/")** "*.rb")
      }

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

