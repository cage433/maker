import sbt._
import java.io.{File, FileInputStream}
import java.util.{Properties => JProperties}


class Starling(info : ProjectInfo) extends ParentProject(info) {


  def project[T <: BasicScalaProject](name : String, construct : ProjectInfo => T, deps : Project*)  : T = project(name, name, construct, deps : _*)
  def project(name : String, deps : Project*) :StarlingProject = project(name, name, new StarlingProject(_), deps : _*)

  lazy val bouncyrmi = project("bouncyrmi", new StarlingProject(_) with BouncyRmiDependencies)
  lazy val utils = project("utils", new StarlingProject(_) with UtilsDependencies)
  lazy val titanScalaModel = project("titan-scala-model", new TitanScalaModel(_))

  // API used to interact with starling, should have minimum number of dependencies
  lazy val starlingApi = {
    // until building the EDM source is fast, or the model is replaced entirely, the following hack
    // is used to prevent non-FC2 builds from compiling the model - which takes a few minutes
    if (starlingProperties.getOrElse("ServerType", "Dev") == "FC2")
      project("starling.api", bouncyrmi, titanScalaModel)
    else
       project("starling.api", new StarlingProject(_) with TitanBinaries, bouncyrmi)
  }

  lazy val auth = project("auth", new StarlingProject(_) with AuthDependencies, utils, bouncyrmi)
  lazy val concurrent = project("concurrent", utils)
  lazy val daterange = project("daterange", utils)
  lazy val quantity = project("quantity", utils)
  lazy val loopyxl = project("loopyxl", new StarlingProject(_) with LoopyXLDependencies, bouncyrmi, auth)
  lazy val maths = project("maths", quantity, daterange)
  lazy val pivot = project("pivot", quantity)
  lazy val pivotUtils = project("pivot.utils", daterange, pivot)
  lazy val guiapi = project("gui.api", new StarlingProject(_) with GuiApiDependencies, pivotUtils, quantity, auth, bouncyrmi)
  lazy val curves = project("curves", maths, pivotUtils, guiapi)
  lazy val instrument = project("instrument", curves)
  lazy val gui = project("gui", new StarlingProject(_) with GuiDependencies, guiapi)
  lazy val trade = project("trade", instrument)
  lazy val VaR = project("var", trade)
  lazy val databases = project("databases", new StarlingProject(_) with DatabasesDependencies, VaR, pivot, guiapi, concurrent, auth, starlingApi)
  lazy val titan = project("titan", new StarlingProject(_) with TitanBinaries, databases)
  lazy val services = project("services", new ServicesProject(_), concurrent, utils, loopyxl, titan)
  lazy val devlauncher = project("dev.launcher", services, gui)

  lazy val starling = this

  override def localScala = {
    defineScala("2.9.0-1.final-local", new File("lib/scala/scala-2.9.0.1.final/")) :: Nil
  }

  var parExec = false
  override def parallelExecution = parExec
  def parallel(b : Boolean){parExec = b}


  lazy val runTest = databases.runTest
  lazy val runRegression = services.runRegression
  lazy val runReadAll= services.runReadAll

  
  class StarlingProject(info : ProjectInfo) extends DefaultProject(info) with com.gu.TeamCityTestReporting {
    override val mainScalaSourcePath = path("src")
    override val testScalaSourcePath = path("tests")
    override def unmanagedClasspath = super.unmanagedClasspath +++ "resources" +++ "test-resources"

    override def repositories = Set()  //Remove dependency on ~/.ivy2 and external http connections
    override def ivyRepositories = List() //Seq(Resolver.defaultLocal(None)) ++ repositories

    def refreshDatabasesAction(args:Array[String]) =
      runTask(Some("starling.utils.RefreshDatabase"), databases.runClasspath, args) dependsOn(compile)

    lazy val refreshDatabase = task {args => refreshDatabasesAction(args)}

    def runOvernightAction(args:Array[String]) =
      runTask(Some("starling.endtoend.EndToEndTest"), testClasspath +++ testCompilePath, args) dependsOn(testCompile)

    lazy val runOvernight = task {args => runOvernightAction(args)}

    lazy val runTest = task { args => runTestNGTest(args(0)) }


    def runScalaTest(className : String) = runTask(
      Some("org.scalatest.tools.Runner"),
      testClasspath +++ testCompilePath, 
      Array("-p", ".", "-eNCOHL", "-s", className)
    ) dependsOn(testCompile)

    def runTestNGTest(className : String) = runTask(
      Some("org.testng.TestNG"), 
      testClasspath +++ testCompilePath, 
      Array("-listener", "starling.utils.SBTTestListener", "-testclass", className)
    ) dependsOn(testCompile)

    lazy val writeClasspathScript = task { 
      // writes a shell script that sets the classpath so I can run from the command line, compile in Vim etc
      import java.io._
      val file = new PrintWriter(new FileOutputStream(new File("set-classpath.sh")))
      file.println("export CLASSPATH=" + devlauncher.testClasspath.getFiles.toList.mkString(":"))
      file.println("export JAVA_OPTS='-server -XX:MaxPermSize=1024m -Xss512k -Xmx6000m'")
      file.close()
      None
    }
  }

  class ServicesProject(info : ProjectInfo) extends StarlingProject(info){
    lazy val runServer = runTask(Some("starling.services.Server"), services.runClasspath, Array[String]()) dependsOn(compile)
    lazy val runRegression = task { args => runTask(
      Some("starling.utils.RegressionRunner"),
      services.runClasspath,
      Array[String]()
    ) dependsOn(compile) }
    lazy val runReadAll = task { args => runTask(
      Some("starling.utils.ReadAll"),
      services.runClasspath,
      Array[String]()
    ) dependsOn(compile) }

    override val unmanagedClasspath =
            super.unmanagedClasspath +++ (Path.fromFile(new File("lib/titan-other-jars")) ** "*.jar")

    override def libraryDependencies = Set(
      "net.liftweb" % "lift-json_2.9.0" % "2.4-M2" withSources(),
      "javax.mail" % "mail" % "1.4" withSources(),
      "javax.servlet" % "servlet-api" % "2.5" withSources(),
      "org.mortbay.jetty" % "jetty" % "6.1.26" withSources(),
      "org.subethamail" % "subethasmtp-wiser" % "1.2" % "test" withSources(),
      "org.subethamail" % "subethasmtp-smtp" % "1.2" % "test" withSources(),
      "org.springframework" % "spring-context-support" % "3.0.5.RELEASE" withSources()
    ) ++ super.libraryDependencies

  }

  class TitanScalaModel(info: ProjectInfo) extends DefaultProject(info) with ModelSourceGeneratingProject {

    private val buildUsingBinaryTooling = true

    val toolingLauncher = if (buildUsingBinaryTooling == true) "../../../mdl/bindinggen.rb" else "/model/tooling/binding-generator/thubc.rb"
    private lazy val projectRoot = path(".").asFile.toString
    val parentPath = Path.fromFile(new java.io.File(projectRoot + "/../../../model/model/"))

    override protected val generateModelMainSourceCmd = Some(new java.lang.ProcessBuilder("ruby", toolingLauncher, "-o", modelMainScalaSourcePath.projectRelativePath, "-b", "../../../mdl/starling/bindings.rb", "../../../mdl/starling/model.rb") directory (new File(projectRoot)))

    lazy val rubyModelPathFinder = {
      (parentPath ** "*.rb")
    }

    lazy val nonModelSourcePath = path("src")
    def copyNonModelSource  = {
      if (! (new java.io.File(projectRoot + "/src").exists)) {
        import FileUtilities._
        val originalSourcePath = Path.fromFile(new java.io.File(parentPath + "/scala-model-with-persistence/src/"))
        copyDirectory(originalSourcePath, nonModelSourcePath, new ConsoleLogger)
        val hibernateBean = new File (projectRoot + "/src/main/scala/com/trafigura/refinedmetals/persistence/CustomAnnotationSessionFactoryBean.scala")
        //println("***** DEBUG ***** path " + hibernateBean.getAbsolutePath + ", " + hibernateBean.exists + ", " + hibernateBean.canWrite) 
        if (hibernateBean.exists && hibernateBean.canWrite) hibernateBean.delete()
      }
      None
    }

    lazy val cleanNonModelSource = task {cleanPath(nonModelSourcePath)}
    override def cleanAction = super.cleanAction dependsOn(cleanNonModelSource)

    lazy val copyNonModelSourceTask = task {copyNonModelSource}
    override def compileAction = super.compileAction dependsOn(
      copyNonModelSourceTask
    )

    def copyJars = {
      import FileUtilities._
      val logger = new ConsoleLogger()
      val srcPath = Path.fromFile(new java.io.File(projectRoot + "/target/scala_2.9.0-1/titan-scala-model_2.9.0-1-1.0.jar"))
      val destPath = Path.fromFile(new java.io.File(projectRoot + "/../lib/titan-model-jars/scala-model-with-persistence.jar"))
      logger.info("copying target jar %s to %s".format(srcPath, destPath))
      val r = copyFile(srcPath, destPath, logger)
      logger.info("copied jar")
      r
    }
    lazy val copyJarsTask = task {copyJars}
    override def packageAction = copyJarsTask dependsOn {
      super.packageAction
    }
    override def libraryDependencies = Set(
      "org.slf4j" % "slf4j-api" % "1.6.1" withSources(),
      "dom4j" % "dom4j" % "1.6.1" withSources(),
      "com.rabbitmq" % "amqp-client" % "1.7.2" withSources(),
      "joda-time" % "joda-time" % "1.6" withSources(),
      "org.codehaus.jettison" % "jettison" % "1.1" withSources(),
      "commons-httpclient" % "commons-httpclient" % "3.1"
    ) ++ super.libraryDependencies
  }

  lazy val starlingProperties = {
    val propsFile = new File("props.conf")
    val p = new JProperties()
    if(propsFile.exists) {
      p.load(new FileInputStream(propsFile))
    }
    // Nastiness as SBT uses scala 2.7
    val javaMap = p.asInstanceOf[java.util.Map[String,String]]
    var result = Map[String, String]()
    val iter = javaMap.keySet.iterator
    while (iter.hasNext){
      val k = iter.next
      result = result + (k -> javaMap.get(k))
    }
    result
  }
  trait UtilsDependencies extends BasicScalaProject{
    override def libraryDependencies = Set(
      "cglib" % "cglib-nodep" % "2.2" withSources(),
      "joda-time" % "joda-time" % "1.6" withSources(),
      "com.rabbitmq" % "amqp-client" % "1.7.2" withSources(),
      "log4j" % "log4j" % "1.2.16" withSources(),
      "org.slf4j" % "slf4j-log4j12" % "1.6.1" withSources(),
      "com.google.collections" % "google-collections" % "1.0" withSources(),
      "commons-codec" % "commons-codec" % "1.4" withSources(),
      "colt" % "colt" % "1.0.3",
      "com.thoughtworks.xstream" % "xstream" % "1.3.1" withSources(),
      "org.testng" % "testng" % "5.8" classifier "jdk15" withSources(),
      // Test dependencies
      "org.mockito" % "mockito-all" % "1.8.2" % "test" withSources(),
      "org.testng" % "testng" % "5.8" classifier "jdk15" withSources()
    ) ++ super.libraryDependencies
}

  trait GuiApiDependencies extends BasicScalaProject {
    override def libraryDependencies = Set(
      "net.debasishg" % "sjson_2.8.0" % "0.8" intransitive() withSources()
    ) ++ super.libraryDependencies
  }
  trait DatabasesDependencies extends BasicScalaProject {
    override def libraryDependencies = Set(
      "org.springframework" % "spring-jdbc" % "3.0.5.RELEASE" withSources(),
      "com.jolbox" % "bonecp" % "0.7.1.RELEASE" intransitive() withSources(),
      "org.slf4j" % "slf4j-api" % "1.6.1" withSources(),
      "org.scala-tools.testing" % "scalacheck_2.9.0-1" % "1.9" withSources(),
      "org.apache.derby" % "derby" % "10.5.3.0_1",
      "hsqldb" % "hsqldb" % "1.8.0.10" % "test",
      "com.h2database" % "h2" % "1.2.131" % "test" withSources()
    ) ++ super.libraryDependencies
  }
  trait AuthDependencies extends BasicScalaProject{
    override def libraryDependencies = Set(
      "com.sun.jna" % "jna" % "3.0.9" withSources()
    ) ++ super.libraryDependencies
  }

  trait LoopyXLDependencies extends BasicScalaProject {
    override def libraryDependencies = Set(
      "com.google.protobuf" % "protobuf-java" % "2.3.0" withSources()
    ) ++ super.libraryDependencies
  }

  trait BouncyRmiDependencies extends BasicScalaProject {
    override def unmanagedClasspath =
      super.unmanagedClasspath +++ Path.fromFile(new File("lib/scala/scala-2.9.0.1.final/lib/scala-swing.jar"))
    override def libraryDependencies = Set(
      "cglib" % "cglib-nodep" % "2.2" withSources(),
      "org.jboss.netty" % "netty" % "3.2.5.Final" withSources(),
      "commons-io" % "commons-io" % "1.3.2" withSources()
    ) ++ super.libraryDependencies
  }

  trait GuiDependencies extends BasicScalaProject{
    override def libraryDependencies = Set(
      "jfree" % "jfreechart" % "1.0.0"
    ) ++ super.libraryDependencies
  }
  trait TitanBinaries extends BasicScalaProject{
    override val unmanagedClasspath =
            super.unmanagedClasspath +++ (Path.fromFile(new File("lib/titan-model-jars")) ** "*.jar")
  }
}