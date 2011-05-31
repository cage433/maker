import sbt._
import java.io.File

class Starling(info : ProjectInfo) extends ParentProject(info) {

  def starlingProject(name : String, dependencies : Project*) = project(name, name, new StarlingProject(name, _), dependencies :_*)
  def swingStarlingProject(name : String, dependencies : Project*) = project(name, name, new SwingStarlingProject(name, _), dependencies :_*)

  // BouncyRMI is largely independent of everything else
  lazy val auth = starlingProject("auth", utils)
  lazy val bouncyrmi = swingStarlingProject("bouncyrmi", utils, auth)

  // projects organised by layer
  lazy val utils = starlingProject("utils")

  lazy val loopyxl = starlingProject("loopyxl", bouncyrmi)
  lazy val concurrent = starlingProject("concurrent", utils)
  lazy val quantity = starlingProject("quantity", utils)
  lazy val daterange = starlingProject("daterange", utils)

  lazy val pivot = starlingProject("pivot", quantity)
  lazy val maths = starlingProject("maths", quantity, daterange)

  lazy val curves = starlingProject("curves", maths, pivot, guiapi)

  lazy val instrument = starlingProject("instrument", curves)

  lazy val trade = starlingProject("trade", instrument)

  lazy val VaR = starlingProject("var", trade)

  lazy val guiapi = starlingProject("gui.api", daterange, pivot, quantity, bouncyrmi)

  lazy val gui = starlingProject("gui", guiapi)

  lazy val databases = starlingProject("databases", VaR, pivot, guiapi, concurrent)

  lazy val services = project("services", "services", new Services(_), databases, concurrent, utils, loopyxl)

  lazy val devlauncher = starlingProject("dev.launcher", services, gui)

//  lazy val webServices = project("starlingWebProject", "starlingWebProject", new DefaultWebProject(_), services)


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
}

