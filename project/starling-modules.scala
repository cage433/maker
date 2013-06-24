import java.util.Properties
import java.io.File
import org.apache.commons.io.FileUtils._
import maker.project._
import maker.project.extras.TmuxMessaging
import maker.project.TopLevelProject
import maker.utils.FileUtils._
import maker.utils.os.Command
import maker.utils.os.Command._
import maker.utils.ModuleId._
import maker.utils.GroupAndArtifact
import maker.task.BuildResult
import maker.MakerProps
import scala.collection.immutable.TreeMap
import maker.utils.ModuleId
import Common._
import Utils._


object Starling {

  println("\n ** Loading (compiled) Starling build...\n")

  import StarlingDependencies._

  lazy val makerProps : MakerProps = MakerProps(file("Maker.conf"))
  lazy val starlingProperties : Map[String, String] = MakerProps.propsFileToMap(file("props.conf"))

  val targetDirName = "target-maker"

  // the following libraries conflict with those in titan poms and break things, they are excluded from the
  // starling build as we provide our own definition of them
  val titanGlobalExclusions = Nil // "org.testng" % "testng" :: "org.slf4j" % "jcl-over-slf4j" :: Nil // currently not building with Titan so this is a non-issue
  val globalDependencies = Dependencies(Nil, titanGlobalExclusions, Nil)

  def project(name : String, upstreamProjects : List[Project], upstreamTestProjects : List[Project]) : Project with MoreSugar = {
    val root = file(name)
    new Project(
      root,
      name,
      layout = new MakerProjectLayout(root),
      upstreamProjects = upstreamProjects,
      upstreamTestProjects = upstreamTestProjects,
      props = makerProps,
      moduleIdentity = Some("starling" % name),
      dependencies = globalDependencies.copy(libs = moduleDependencies(name))
    ) with TmuxMessaging with MoreSugar
  }

  def reportRedundantJars(){
    val projects = starling.allUpstreamProjects
    val jarsByProject = projects.map{
      p ⇒ (p, p.classpathJarsOnly.map(_.getName).filterNot(_.contains("scala-library")).toSet)
    }.toMap
    projects.foreach{
      var redundancies = TreeMap[String, List[String]]()
      p ⇒ 
        jarsByProject(p).foreach{
          jarName ⇒ 
            p.allStrictlyUpstreamProjects.filter{
              u ⇒ 
                jarsByProject(u).contains(jarName)
            } match {
              case Nil ⇒ 
              case ps ⇒ redundancies += (jarName -> ps.map(_.name))
            }
        }
        if (redundancies.nonEmpty){
          val errorLines = redundancies.map{
            case (jarName, upstreams) => "\t " + jarName + " is contained in " + upstreams.mkString(", ")
          }
          println(errorLines.mkString("Project " + p + " has reduntant jars\n", "\n", ""))
        }
        redundancies = TreeMap[String, List[String]]()
    }
  }

  trait MoreSugar{
    self : Project ⇒
      def tcc = testCompileContinuously
      def stfe {
        props.ShowFailingTestException := ! props.ShowFailingTestException()
      }
  }

  def project(name : String, upstreamProjects : Project*) : Project = project(name, upstreamProjects.toList, Nil)

  // projects ordered by layer, then alphabetically, each layer depends only on layers above
  // the dependencies of each project are also ordered by their position in this list.
  // OCD yes but helps to visualise the project. There is a school of thought that too many layers is bad, we have 18!

  // Layer 1
  lazy val booter = project("booter")
  lazy val loopyxlJava = project("loopyxl-java")
  lazy val utils = project("utils")
  // Layer 2
  lazy val browserService = project("browser.service", utils)
  // Layer 3
  lazy val browser = project("browser", browserService)
  lazy val props = project("props", utils)
  lazy val starlingDTOApi = project("starling.dto.api", utils)
  // Layer 4
  lazy val daterange = project("daterange", starlingDTOApi)
  lazy val quantity = project("quantity", List(starlingDTOApi), List(utils))
  // Layer 5
  lazy val auth = project("auth", daterange)
  lazy val dbx = project("dbx", props, daterange)
  lazy val maths = project("maths", List(daterange, quantity), List(daterange, quantity))
  lazy val pivot = project("pivot", List(quantity), List(utils))
  // Layer 6
  lazy val bouncyrmi = project("bouncyrmi", List(auth), List(utils))
  lazy val loopyxl = project("loopyxl", loopyxlJava, auth)
  lazy val pivotUtils = project("pivot.utils", daterange, pivot)
  lazy val schemaevolution = project("schemaevolution", dbx) // Please do not change this dependency without asking
  // Layer 7
  lazy val guiapi = project("gui.api", browserService, bouncyrmi, pivotUtils, loopyxlJava)
  lazy val starlingClient = project("starling.client", bouncyrmi)
  // Layer 8
  lazy val curves = project("curves", List(maths, guiapi), List(daterange, quantity))
  lazy val eventViewerApi = project("event.viewer.api", guiapi)
  lazy val fc2Facility = project("fc2.facility",  guiapi)
  lazy val reportsFacility = project("reports.facility", guiapi)
  lazy val tradeFacility = project("trade.facility", guiapi)
  // Layer 9
  lazy val gui = project("gui", browser, eventViewerApi, fc2Facility, reportsFacility, tradeFacility)
  lazy val instrument = project("instrument", List(curves), List(maths, curves))
  // Layer 10
  lazy val databases = project("databases", List(dbx, instrument), List(curves))
  // Layer 11
  lazy val services = project("services", List(loopyxl, fc2Facility, reportsFacility, databases), List(instrument))
  // Layer 12
  lazy val tradeImpl = project("trade.impl", tradeFacility, services)
  lazy val pnlreconcile = project("pnlreconcile", List(tradeFacility, services), List(schemaevolution, curves))
  lazy val webservice = project("webservice", List(services), List(utils))
  // Layer 13
  lazy val eventstoreServer = project("eventstore-server", List(webservice, schemaevolution), List(utils))
  lazy val reportsImpl = project("reports.impl", List(schemaevolution, pnlreconcile), List(databases))
  // Layer 14
  lazy val oil = project("oil", webservice, reportsImpl)
  lazy val titan = project("titan", List(starlingClient, webservice, reportsImpl), List(instrument, webservice))
  // Layer 15
  lazy val eventViewerService = project("event.viewer.service", eventViewerApi, titan)
  // Layer 16
  lazy val startserver = project("startserver", tradeImpl, oil, eventViewerService)
  // Layer 17
  lazy val launcher = project("launcher", List(booter, gui, startserver), List(curves))
  // Layer 18
  lazy val starling = new TopLevelProject("starling", List(launcher, eventstoreServer), makerProps,
    List(
      "logs",
      "osgi-gui-cache",
      "osgi-server-cache",
      "modulejarcache",
      ".maker",
      "target",
      "test-output",
      "maker/dist",
      "component-tests",
      "refined-service",
      "refinedtestclient"
    )) with TmuxMessaging with MoreSugar

  reportRedundantJars()

  // below are some utils for running starling from maker
  def stdRunner(proj : Project)(className : String) = {
    proj.compile
    proj.runMain(className)(commonLaunchArgs : _*)()
  }
  val launcherRunner = stdRunner(launcher) _
  def runLauncher = launcherRunner("starling.launcher.Launcher")
  def runDevLauncher = launcherRunner("starling.launcher.DevLauncher")
  def runServer = launcherRunner("starling.startserver.Server")

  def writeStarlingClasspath() {
    val cp = launcher.compilePhase.classpathDirectoriesAndJars
    val classpathString = cp.map(_.relativeTo(file("."))).map(_.getPath).filterNot(_.endsWith("-sources.jar")).toList.sortWith(_<_).mkString(":")
    writeToFile(file("bin/deploy-classpath.sh"), "export CLASSPATH=" + classpathString)
  }

  def writeStarlingClasspathWithTests() {
    val cp = launcher.testCompilePhase.classpathDirectoriesAndJars
    val classpathString = cp.map(_.relativeTo(file("."))).map(_.getPath).filterNot(_.endsWith("-sources.jar")).toList.sortWith(_<_).mkString(":")
    writeToFile(file("bin/deploy-classpath.sh"), "export CLASSPATH=" + classpathString)
  }

  def publishLocalStarlingDeps(version : String) = {
    starlingClient.update
    quantity.update
    daterange.update
    starlingClient.Publish("local-m2-publish", version).execute
    quantity.Publish("local-m2-publish", version).execute
    daterange.Publish("local-m2-publish", version).execute
  }

  object StarlingDependencies {
    val moduleDependencies = Map(
      "starling" -> List(
      ),
      "launcher" -> List(
         "starling.test.resources" % "bookmark-test-db" % "1.2",
         "org.suecarter" % "tablediff-2.9" % "0.2.3"
      ),
      "auth" -> List(
         "net.java.dev.jna" % "jna" % "3.3.0",
         "net.java.dev.jna" % "jna-platform" % "3.3.0"
      ),
      "booter" -> List(

      ),
      "bouncyrmi" -> List(
         "org.jboss.netty" % "netty" % "3.2.5.Final",
         "org.slf4j" % "jul-to-slf4j" % "1.7.2",
         "org.slf4j" % "jcl-over-slf4j" % "1.7.2",
         "javax.servlet" % "servlet-api" % "2.5"
      ),
      "browser" -> List(
         "jxlayer" % "jxlayer" % "4.0",
         "jgoodies" % "looks" % "2.3.1",
         "org.swinglabs" % "swingx-core" % "1.6.2-2",
         "mig-swing" % "miglayout" % "4.0",
         "net.java.dev.timingframework" % "timingframework" % "1.0",
         "starling-external-jars" % "org.eclipse.mylyn.wikitext.textile.core" % "1.4",
         "starling-external-jars" % "org.eclipse.mylyn.wikitext.core" % "1.4" %% "e3x"
      ),
      "browser.service" -> List(
         "org.scala-lang" % "scala-compiler" % "2.9.2"
      ),
      "curves" -> List(

      ),
      "databases" -> List(
         "com.trafigura.tradeservice" % "tradeservice-api" % "${titan_binary_version}",
         "com.trafigura.tradeservice" % "titan-edm" % "${titan_binary_version}",
         "com.trafigura.tradeservice" % "titan-core-java" % "${titan_binary_version}",
         "com.trafigura.tradeservice" % "trademanagement-edm" % "${titan_binary_version}",
         "com.trafigura.tradeservice" % "pricingmanagement-api" % "${titan_binary_version}",
         "com.trafigura.tradeservice" % "pricingmanagement-edm" % "${titan_binary_version}",
         "com.trafigura.tradeservice" % "common-edm" % "${titan_binary_version}",
         "com.trafigura.tradeservice" % "srd-edm" % "${titan_binary_version}",
         "com.trafigura.titan.shared-libs" % "referencedatareadonly-lib" % "${titan_binary_version}",
         "com.trafigura.tradinghub" % "persistence-support" % "2.28.1",
         "starling.test.resources" % "aspect-test-20120927" % "1",
         "starling.test.resources" % "aspect-test-20121002" % "1",
         "org.scala-lang" % "scalap" % "2.9.2",
         "net.liftweb" % "lift-json_2.9.2" % "2.4",
         "com.thoughtworks.paranamer" % "paranamer" % "2.3",
         "org.springframework" % "spring-asm" % "3.1.0.RELEASE",
         "org.apache.derby" % "derby" % "10.5.3.0_1",
         "hsqldb" % "hsqldb" % "1.8.0.10",
         "com.h2database" % "h2" % "1.3.163",
         "starling-external-jars" % "mimapi" % "2.2.0",
         "org.acplt" % "oncrpc" % "1.0.7",
         "org.hibernate" % "hibernate-annotations" % "3.5.3-Final",
         "org.hibernate.javax.persistence" % "hibernate-jpa-2.0-api" % "1.0.0.Final",
         "org.hamcrest" % "hamcrest-library" % "1.1",
         "org.hamcrest" % "hamcrest-core" % "1.1",
         "org.apache.mahout" % "mahout-math" % "0.7"
      ),
      "daterange" -> List(

      ),
      "dbx" -> List(
         "com.jolbox" % "bonecp" % "0.7.1.RELEASE",
         "org.springframework" % "spring-jdbc" % "3.1.0.RELEASE",
         "jtds" % "jtds" % "1.2.5",
         "com.oracle" % "ojdbc6" % "11.2.0.1.0",
         "org.springframework" % "spring-tx" % "3.1.0.RELEASE",
         "org.springframework" % "spring-core" % "3.1.0.RELEASE",
         "org.springframework" % "spring-beans" % "3.1.0.RELEASE",
         "org.slf4j" % "jcl-over-slf4j" % "1.7.2"
      ),
      "event.viewer.api" -> List(

      ),
      "event.viewer.service" -> List(

      ),
      "fc2.facility" -> List(

      ),
      "gui" -> List(
         "jfree" % "jfreechart" % "1.0.13",
         "jfree" % "jcommon" % "1.0.16",
         "org.logback" % "logback-gelf" % "1.1"
      ),
      "gui.api" -> List(
         "net.debasishg" % "sjson_2.9.2" % "0.19",
         "net.databinder" % "dispatch-json_2.9.2" % "0.8.9"
      ),
      "instrument" -> List(

      ),
      "loopyxl" -> List(
        "starling-external-jars" % "xlloop" % "0.3.1"
      ),
      "loopyxl-java" -> List(
         "com.google.protobuf" % "protobuf-java" % "2.4.1"
      ),
      "maths" -> List(
         "org.apache.commons" % "commons-math3" % "3.0"
      ),
      "oil" -> List(

      ),
      "pivot" -> List(
         "com.googlecode.javaewah" % "JavaEWAH" % "0.3.3"
      ),
      "pivot.utils" -> List(

      ),
      "pnlreconcile" -> List(
         "commons-lang" % "commons-lang" % "2.5"
      ),
      "props" -> List(

      ),
      "quantity" -> List(

      ),
      "reports.facility" -> List(

      ),
      "reports.impl" -> List(
         "starling.test.resources" % "snapshot-20120613-zip" % "1",
         "starling.test.resources" % "snapshot-20120613-gz" % "1",
         "starling.test.resources" % "snapshot-20120613-xml" % "1",
         "starling.test.resources" % "GasOilSpec_Yearly_1011.txt" % "1",
         "starling.test.resources" % "NaphthaSpec_Yearly_1011.txt" % "1"
      ),
      "schemaevolution" -> List(

      ),
      "services" -> List(
         "javax.mail" % "mail" % "1.4",
         "org.mortbay.jetty" % "jetty" % "6.1.26",
         "org.mortbay.jetty" % "jetty-util" % "6.1.26",
         "org.subethamail" % "subethasmtp-wiser" % "1.2",
         "org.subethamail" % "subethasmtp-smtp" % "1.2",
         "org.springframework" % "spring-context-support" % "3.1.0.RELEASE",
         "org.springframework" % "spring-context" % "3.1.0.RELEASE",
         "commons-httpclient" % "commons-httpclient" % "3.1",
         "org.jboss.resteasy" % "resteasy-jaxrs" % "2.2.2.GA",
         "org.scannotation" % "scannotation" % "1.0.3",
         "net.databinder" % "dispatch-http_2.9.2" % "0.8.9",
         "net.databinder" % "dispatch-core_2.9.2" % "0.8.9",
         "org.apache.httpcomponents" % "httpclient" % "4.1.2",
         "org.apache.httpcomponents" % "httpcore" % "4.1.2",
         "com.trafigura.titan.shared-libs" % "titan-core" % "${titan_binary_version}",
         "com.trafigura.titan.shared-libs" % "titan-security" % "${titan_binary_version}"
         // "com.trafigura.titan" % "model-trademgmt-internal-scala-bindings" % "1.5"
      ),
      "starling.client" -> List(
         "com.trafigura.titan.shared-libs" % "titan-framework" % "${titan_binary_version}"
      ),
      "starling.dto.api" -> List(
         "org.jboss.resteasy" % "jaxrs-api" % "2.2.2.GA",
         "com.trafigura.titan" % "model-common-public-scala-bindings" % "${titan_binary_version}",
         "com.trafigura.tradinghub" % "scala-hub-support" % "2.28.1",
         "org.codehaus.jettison" % "jettison" % "1.3.1"
      ),
      "startserver" -> List(

      ),
      "titan" -> List(
         "com.trafigura.tradeservice" % "titan-api-core" % "${titan_binary_version}",
         "com.trafigura.tradeservice" % "pricingmanagement-api-ws" % "${titan_binary_version}",
         "com.trafigura.tradeservice" % "tradeservice-core" % "${titan_binary_version}",
         "com.trafigura.tradeservice" % "tradeservice-api-testutils" % "${titan_binary_version}",
         "com.trafigura.tradeservice" % "trademanagement-api-gs" % "${titan_binary_version}",
         "com.trafigura.tradeservice" % "trademanagement-gs" % "${titan_binary_version}",
         "com.trafigura.titan" % "model-logistics-public-scala-bindings" % "${titan_binary_version}",
         "com.trafigura.services" % "logistics-edm" % "${titan_binary_version}",
         "com.trafigura.services" % "logistics-api" % "${titan_binary_version}",
         "com.trafigura.services" % "logistics-api-ws" % "${titan_binary_version}",
         "com.trafigura.titan" % "model-pricingmanagement-public-scala-bindings" % "${titan_binary_version}",
         "com.trafigura.services" % "pricingmanagement-api" % "${titan_binary_version}",
         // "com.trafigura.titan" % "model-trademgmt-public-scala-bindings" % "1.5",
         "com.oracle" % "aqapi" % "11.2.0",
         "org.javatuples" % "javatuples" % "1.2",
         "aopalliance" % "aopalliance" % "1.0",
         "javax.validation" % "validation-api" % "1.0.0.GA",
         "commons-dbcp" % "commons-dbcp" % "1.3",
         "org.codehaus.castor" % "castor" % "1.2",
         "commons-pool" % "commons-pool" % "1.5.6",
         "org.codehaus.jackson" % "jackson-mapper-asl" % "1.9.4",
         "org.codehaus.jackson" % "jackson-core-asl" % "1.9.4",
         "org.hibernate" % "hibernate-core" % "3.5.3-Final",
         "org.hibernate" % "hibernate-commons-annotations" % "3.2.0.Final",
         "org.hibernate" % "hibernate-entitymanager" % "3.5.3-Final",
         "org.hibernate" % "hibernate-jmx" % "3.5.3-Final",
         "org.hibernate" % "hibernate-validator" % "4.2.0.Final",
         "javax.transaction" % "jta" % "1.1",
         "antlr" % "antlr" % "2.7.6",
         "org.springframework" % "spring-aspects" % "3.1.0.RELEASE",
         "org.springframework" % "spring-oxm" % "3.1.0.RELEASE",
         "org.springframework" % "spring-web" % "3.1.0.RELEASE",
         "org.springframework" % "spring-orm" % "3.1.0.RELEASE",
         "org.springframework" % "spring-aop" % "3.1.0.RELEASE",
         "org.springframework.security" % "spring-security-config" % "3.1.0.RELEASE",
         "org.springframework.security" % "spring-security-core" % "3.1.0.RELEASE",
         "org.springframework.security" % "spring-security-web" % "3.1.0.RELEASE",
         "org.springframework.ldap" % "spring-ldap-core" % "1.3.1.RELEASE",
         "org.springframework" % "spring-expression" % "3.1.0.RELEASE",
         "org.springframework.retry" % "spring-retry" % "1.0.0.RELEASE",
         "org.springframework.batch" % "spring-batch-core" % "2.1.8.RELEASE",
         "org.springframework.batch" % "spring-batch-infrastructure" % "2.1.8.RELEASE",
         "org.springframework" % "spring-aop" % "3.1.0.RELEASE",
         "org.apache.cxf" % "cxf-rt-frontend-jaxrs" % "2.6.0",
         "org.apache.cxf" % "cxf-rt-transports-http-jetty" % "2.6.0",
         "org.apache.cxf" % "cxf-rt-transports-http" % "2.6.0",
         "org.apache.cxf" % "cxf-rt-frontend-jaxws" % "2.6.0",
         "org.apache.cxf" % "cxf-rt-databinding-jaxb" % "2.6.0",
         "org.apache.cxf" % "cxf-rt-ws-policy" % "2.6.0",
         "org.apache.cxf" % "cxf-rt-ws-addr" % "2.6.0",
         "org.apache.cxf" % "cxf-rt-transports-jms" % "2.6.0",
         "org.apache.cxf" % "cxf-rt-management" % "2.6.0",
         "org.apache.cxf" % "cxf-rt-frontend-simple" % "2.6.0",
         "org.apache.cxf" % "cxf-rt-core" % "2.6.0",
         "org.apache.cxf" % "cxf-rt-bindings-xml" % "2.6.0",
         "org.apache.cxf" % "cxf-rt-bindings-soap" % "2.6.0",
         "org.apache.cxf" % "cxf-api" % "2.6.0",
         "commons-collections" % "commons-collections" % "3.2",
         "wsdl4j" % "wsdl4j" % "1.6.2",
         "org.apache.neethi" % "neethi" % "3.0.2",
         "org.apache.ws.xmlschema" % "xmlschema-core" % "2.0.2",
         "com.gigaspaces" % "gs-openspaces" % "9.1.0-RELEASE",
         "com.gigaspaces" % "gs-runtime" % "9.1.0-RELEASE"
      ),
      "trade.facility" -> List(

      ),
      "trade.impl" -> List(

      ),
      "utils" -> List(
         "ch.qos.logback" % "logback-classic" % "1.0.6",
         "ch.qos.logback" % "logback-core" % "1.0.6",
         "org.slf4j" % "slf4j-api" % "1.7.2",
         "net.liftweb" % "lift-json_2.9.2" % "2.5-M4",
         "joda-time" % "joda-time" % "2.1",
         "org.joda" % "joda-convert" % "1.2",
         "com.rabbitmq" % "amqp-client" % "1.7.2",
         "com.google.guava" % "guava" % "12.0",
         "com.google.code.findbugs" % "jsr305" % "2.0.0",
         "commons-codec" % "commons-codec" % "1.5",
         "commons-io" % "commons-io" % "1.4",
         "colt" % "colt" % "1.0.3",
         "org.apache.mahout" % "mahout-collections" % "1.0",
         "com.thoughtworks.xstream" % "xstream" % "1.4.2",
         "redis.clients" % "jedis" % "2.0.0",
         "org.mockito" % "mockito-all" % "1.8.2",
         "org.jmock" % "jmock" % "2.5.1",
         "junit" % "junit" % "4.8.2",
         "org.scalacheck" % "scalacheck_2.9.2" % "1.9",
         "com.yourkit" % "yjp-controller-api-redist" % "UNKNOWN",
         "org.netbeans" % "insane" % "UNKNOWN",
         "org.netbeans.api" % "org-netbeans-modules-nbjunit" % "UNKNOWN",
         "com.twitter" % "finagle-core_2.9.2" % "6.0.5",
         "com.twitter" % "finagle-stream_2.9.2" % "6.0.5",
         "com.twitter" % "finagle-http_2.9.2" % "6.0.5",
         "com.twitter" % "util-core_2.9.2" % "6.0.5",
         "com.twitter" % "util-codec_2.9.2" % "6.0.5",
         "com.twitter" % "util-jvm_2.9.2" % "6.0.5",
         "org.scala-lang" % "scala-swing" % "2.9.2",
         "com.beust" % "jcommander" % "1.12",
         "org.beanshell" % "bsh" % "2.0b4",
         "org.testng" % "testng" % "6.2.1",
         "org.scala-tools" % "scala-stm_2.9.2" % "0.5",
         "cglib" % "cglib-nodep" % "2.2",
         "org.scalatest" % "scalatest_2.9.2" % "1.8",
         "org.scalaz" % "scalaz-core_2.9.2" % "6.0.4",
         "org.scalamock" % "scalamock-core_2.9.2" % "2.4",
         "org.scalamock" % "scalamock-scalatest-support_2.9.2" % "2.4",
         "com.google.inject" % "guice" % "2.0"
      ),
      "webservice" -> List(
         "dom4j" % "dom4j" % "1.6.1",
         "javassist" % "javassist" % "3.9.0.GA",
         "dom4j" % "dom4j" % "1.6.1"
      ),
      "eventstore-server" -> List(
         "com.h2database" % "h2" % "1.3.170",
         "com.twitter" % "util-eval_2.9.2" % "6.0.5",
         "net.lshift.diffa" % "participant-support" % "1.5.10",
         "org.springframework" % "spring-web" % "3.1.0.RELEASE",
         "org.codehaus.jackson" % "jackson-core-asl" % "1.9.4",
         "org.codehaus.jackson" % "jackson-mapper-asl" % "1.9.4"
      )
    ) withDefaultValue(Nil) // for now this can be ommited since ivy files are still supported, make more strict when we stop reading ivy altogether...
  }
}
