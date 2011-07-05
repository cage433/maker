import sbt._
import java.io.{File, FileInputStream}
import java.util.{Properties => JProperties}


class Starling(info : ProjectInfo) extends ParentProject(info) {

  def starlingProject(name : String, dependencies : Project*) = project(name, name, new StarlingProject(name, _), dependencies :_*)
  def swingStarlingProject(name : String, dependencies : Project*) = project(name, name, new SwingStarlingProject(name, _), dependencies :_*)

  lazy val bouncyrmi = swingStarlingProject("bouncyrmi")
  lazy val utils = starlingProject("utils")
  lazy val scalaModelWithPersistence = project("titan-scala-model-with-persistence", "ScalaModelWithPersistence", new ScalaModelWithPersistence(_))

  lazy val auth = starlingProject("auth", utils, bouncyrmi)
  lazy val concurrent = starlingProject("concurrent", utils)
  lazy val daterange = starlingProject("daterange", utils)
  lazy val quantity = starlingProject("quantity", utils)
  lazy val starlingApi = {
    // until building the EDM source is fast, or the model is replaced entirely, the following hack
    // is used to prevent non-FC2 builds from compiling the model - which takes a few minutes
    val name = "starling.api"
    if (starlingProperties.getOrElse("ServerType", "Dev") == "FC2")
      project(name, name, {info : ProjectInfo => new StarlingProject(name, info)}, bouncyrmi, scalaModelWithPersistence)
    else
      project(name, name, new StarlingProject(name, _){override val unmanagedClasspath = super.unmanagedClasspath +++ ("scala-model-jars" ** "*.jar")}, bouncyrmi)
  }

  lazy val loopyxl = starlingProject("loopyxl", bouncyrmi, auth)

  lazy val maths = starlingProject("maths", quantity, daterange)
  lazy val pivot = starlingProject("pivot", quantity)

  lazy val curves = starlingProject("curves", maths, pivot, guiapi)
  lazy val guiapi = starlingProject("gui.api", daterange, pivot, quantity, auth, bouncyrmi)

  lazy val instrument = starlingProject("instrument", curves)
  lazy val gui = starlingProject("gui", guiapi)

  lazy val trade = starlingProject("trade", instrument)

  lazy val VaR = starlingProject("var", trade)

  lazy val databases = starlingProject("databases", VaR, pivot, guiapi, concurrent, auth)

  lazy val services = project("services", "services", new Services(_), databases, concurrent, utils, loopyxl, starlingApi)

  lazy val devlauncher = starlingProject("dev.launcher", services, gui)

  lazy val starling = this

  override def localScala = {
    defineScala("2.8.1.final-local", new File("lib/scala/scala-2.8.1.final/")) :: Nil
  }

  var parExec = false
  override def parallelExecution = parExec
  def parallel(b : Boolean){parExec = b}


  lazy val runTest = databases.runTest
  lazy val runRegression = services.runRegression
  lazy val runReadAll= services.runReadAll

  class SwingStarlingProject(name : String, info : ProjectInfo) extends StarlingProject(name, info) {
    override def unmanagedClasspath =
      super.unmanagedClasspath +++ Path.fromFile(new File("lib/scala/scala-2.8.1.final/lib/scala-swing.jar"))
  }

  class StarlingProject(name : String, info : ProjectInfo) extends DefaultProject(info) with com.gu.TeamCityTestReporting {
    override val mainScalaSourcePath = path("src")
    override val testScalaSourcePath = path("tests")
    override val dependencyPath = path("jars") 
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
      file.println("export CLASSPATH=" + services.testClasspath.getFiles.toList.mkString(":"))
      file.println("export JAVA_OPTS='-server -XX:MaxPermSize=512m -Xss128k -Xmx6000m'")
      file.close()
      None
    }
  }

  class Services(info : ProjectInfo) extends StarlingProject("services", info){
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


  }


  class ScalaModelWithPersistence(info: ProjectInfo) extends DefaultProject(info) with ModelSourceGeneratingProject with ModelDependencies{

    private lazy val projectRoot = path(".").asFile.toString

    override protected val generateModelMainSourceCmd = Some(new java.lang.ProcessBuilder("ruby", "../../../model/model/bindinggen.rb", "-o", modelMainScalaSourcePath.projectRelativePath, "-b", "../../../model/model/scala-model-with-persistence/src/codegen/bindings.rb", "../../../model/model/master-model.rb") directory (new File(projectRoot)))

    lazy val rubyModelPathFinder = {
      val parentPath = Path.fromFile(new java.io.File(projectRoot + "/../../../model/model/"))
      (parentPath ** "*.rb")
    }

    lazy val nonModelSourcePath = path("src")
    def copyNonModelSource  = {
      if (! (new java.io.File(projectRoot + "/src").exists)) {
        import FileUtilities._
        val originalSourcePath = Path.fromFile(new java.io.File(projectRoot + "../../../../model/model/scala-model-with-persistence/src/"))
        copyDirectory(originalSourcePath, nonModelSourcePath, new ConsoleLogger)
      }
      None
    }

    lazy val cleanNonModelSource = task {cleanPath(nonModelSourcePath)}
    override def cleanAction = super.cleanAction dependsOn(cleanNonModelSource)

    lazy val copyNonModelSourceTask = task {copyNonModelSource}
    override def compileAction = super.compileAction dependsOn(
      copyNonModelSourceTask
    )
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
    //Map() ++ JavaConversions.asScalaMap(p.asInstanceOf[java.util.Map[String,String]])
  }
}

