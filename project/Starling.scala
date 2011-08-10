import sbt._
import Keys._
import java.io.File

object Dependencies{
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
    "org.testng" % "testng" % "5.8" classifier "jdk15" withSources(),
    // Test dependencies
    "org.mockito" % "mockito-all" % "1.8.2" % "test" withSources(),
    "org.testng" % "testng" % "5.8" classifier "jdk15" withSources()
  ) 

  val bouncyRmiDependencies = Seq(
    "cglib" % "cglib-nodep" % "2.2" withSources(),
    "org.jboss.netty" % "netty" % "3.2.3.Final" withSources(),
    "commons-io" % "commons-io" % "1.3.2" withSources(),
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
}

object StarlingBuild extends Build{

  import Dependencies._

  lazy val standardSettings = Defaults.defaultSettings ++ Seq(
    unmanagedSourceDirectories in Compile <+= baseDirectory(_/"src"),
    unmanagedBase <<= baseDirectory( (base: File) => base /"lib"),
    scalaVersion := "2.9.0-1"
  )



  lazy val utils = Project(
    "utils", 
    file("./utils"), 
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
  ) dependsOn (utils) //;w

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
  ) dependsOn(quantity, daterange)

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
  ) dependsOn(pivotUtils, quantity, auth, bouncyrmi)

  lazy val curves = Project(
    "curves", 
    file("./curves"),
    settings = standardSettings
  ) dependsOn(maths, pivotUtils,guiapi)

  lazy val instrument = Project(
    "instrument", 
    file("./instrument"),
    settings = standardSettings
  ) dependsOn(curves)

  lazy val gui = Project(
    "gui", 
    file("./gui"),
    settings = standardSettings ++ Seq(libraryDependencies ++= guiDependencies)
  ) dependsOn(guiapi)

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



//lazy val databases = Project(
  //"databases", 
  //file("./databases"),
  //settings = standardSettings ++ Seq(libraryDependencies ++= guiDependencies)
  //) dependsOn(guiapi)

  lazy val root = Project("starling", file("."), settings = standardSettings) aggregate (
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
    curves,
    instrument,
    gui,
    trade,
    VaR
  )

  // titan model project definition
  import TitanModel._

  val modelGenSrcDir = file("titan-scala-model/model-src/main/scala/")
  val copiedSrcDir = file("titan-scala-model/src")
  val modelRoot = file("titan-scala-model")
  def cleanGenSrc = IO.delete(modelGenSrcDir) 
  def cleanCopiedSrc = IO.delete(copiedSrcDir) 

  lazy val titanModel = Project(
    "titan-model", 
    modelRoot,
    settings = standardSettings ++ Seq(
      sourceGenerators in Compile <+= sourceManaged in Compile map { dir =>
        TitanModel.buildSource(modelRoot, modelGenSrcDir)
      },
      libraryDependencies ++= titanModelDependencies,
      resolvers ++= Seq(
        "Trafigura Nexus Repository" at "http://nexus.global.trafigura.com:8081/nexus/content/repositories/tooling-releases/",
        "Alfresco (needed for resteasy 1.2)" at "http://maven.alfresco.com/nexus/content/groups/public/"
      ),
      cleanGenSrcTask := cleanGenSrc, 
      cleanCopiedSrcTask := cleanCopiedSrc, 
      clean <<= clean.dependsOn(cleanGenSrcTask, cleanCopiedSrcTask)
    )
  )
 

  object TitanModel {
    
    val cleanGenSrcTask = TaskKey[Unit]("clean-src", "Clean model generated sources")
    val cleanCopiedSrcTask = TaskKey[Unit]("clean-copied-src", "Clean sources copied from model")
     
    def buildSource(titanModuleRoot : File, outputDir :File) : Seq[File] = {
      println("Outputting model gen src to " + outputDir)
      def latestRubyFileTime = {
        val files = rubyModelPathFinder.getFiles
        if (files.isEmpty)
          throw new Exception("No ruby files found")
        files.map(_.lastModified).toList.sort(_>_).head
      }
      
      def earliestScalaFileTime = {
        (outputDir ** "*.scala").getFiles.toList.map(_.lastModified).sort(_<_) match {
          case Nil => None
          case t :: _ => Some(t)
        }
      }
      lazy val buildUsingBinaryTooling = true

      val toolingLauncher = if (buildUsingBinaryTooling == true) new File(titanModuleRoot, "../../../mdl/bindinggen.rb") else new File(titanModuleRoot, "/model/tooling/binding-generator/thubc.rb")

      val generateModelMainSourceCmd = new java.lang.ProcessBuilder("ruby", toolingLauncher.getAbsolutePath, "-o", outputDir.getAbsolutePath, "-b", "../../../mdl/starling/bindings.rb", "../../../mdl/starling/model.rb") directory titanModuleRoot

      lazy val rubyModelPathFinder = {
        (new File(titanModuleRoot, "/../../../model/model/")** "*.rb")
      }

      lazy val nonModelSourcePath = new File(titanModuleRoot, "src")
      def copyNonModelSource  = {
        if (! (nonModelSourcePath.exists)) {
          import IO._
          val originalSourcePath = new File(titanModuleRoot, "../../../model/model/scala-model-with-persistence/src/")
          copyDirectory(originalSourcePath, nonModelSourcePath)
          val hibernateBean = new File (titanModuleRoot, "/src/main/scala/com/trafigura/refinedmetals/persistence/CustomAnnotationSessionFactoryBean.scala")
          println("***** DEBUG ***** path " + hibernateBean.getAbsolutePath + ", " + hibernateBean.exists + ", " + hibernateBean.canWrite) 
          if (hibernateBean.exists && hibernateBean.canWrite) hibernateBean.delete()
        }
        None
      }

      (latestRubyFileTime, earliestScalaFileTime) match {
        case (t_ruby, Some(t_scala)) if t_ruby < t_scala => Seq[File]()
        case _ => copyNonModelSource; generateModelMainSourceCmd !; (outputDir ** "*.scala").get ++ (nonModelSourcePath ** "*.scala").get
      }      
    }
  }
}

