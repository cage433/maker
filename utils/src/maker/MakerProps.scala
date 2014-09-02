package maker

import scala.collection.mutable.{Map => MMap}
import java.io.File
import java.util.Properties
import scala.collection.JavaConversions
import maker.utils.MakerLog
import java.io.FileInputStream
import maker.utils.FileUtils._
import maker.utils.FileUtils


case class MakerProps (overrides : MMap[String, String]) extends PropsTrait{
  
  lazy val log = {
    val log = MakerLog()
    log
  }

  object MakerHome extends SystemProperty("maker.home") with IsString
  object MakerTestReporterJar extends Default(MakerHome() + "/maker-scalatest-reporter.jar") with IsFile


  case class ExternalResourceConfig(configFile : File){
    private def extractMap(prefix : String) : Map[String, String] = {
      configFile.readLines.filter(_.startsWith(prefix)).map{
        line => 
          val List(key, value) = line.split(' ').filterNot(_.isEmpty).drop(1).toList
          key -> value
      }.toMap
    }
    def resourceVersions() = extractMap("version:")
    def resourceResolvers() = extractMap("resolver:")
  }

  import MakerProps._
  object ScalaHome extends EnvProperty("SCALA_HOME") with IsFile
  object JavaHome extends EnvProperty("JAVA_HOME", "JDK_HOME") with IsFile
  object Java extends Default(JavaHome() + "/bin/java") with IsFile
  object Javac extends Default(JavaHome() + "/bin/javac") with IsFile
  object Jar extends Default(JavaHome() + "/bin/jar") with IsFile
  object MakerScalaVersion extends Default(DefaultScalaVersion) with IsString
  object ProjectScalaVersion extends Default(DefaultScalaVersion) with IsString
  object HomeDir extends SystemProperty("user.home") with IsFile
  object VimErrorFile extends Default("vim-compile-output") with IsFile
  object GroupId extends Property with IsString
  object Compiler extends Default("scalac") with IsString
  object ExternalResourceConfigFile extends Default(file("external-resource-config")) with IsFile
  def resourceVersions() : Map[String, String] = ExternalResourceConfig(ExternalResourceConfigFile()).resourceVersions()
  def resourceResolvers() : Map[String, String] = ExternalResourceConfig(ExternalResourceConfigFile()).resourceResolvers()
  def defaultResolver() : String = resourceResolvers.getOrElse("default", throw new RuntimeException("No default resolver"))

  /**
   * Maker has its own logback file which applies during compilation, 
   * this is the one that is used when running tests and main methods
   */
  object LogbackTestConfigFile extends SystemPropertyWithDefault("maker.test.logback.config", file("logback-unit-tests.xml")) with IsFile

  object ProjectScalaLibraryJar extends Default(file("scala-libs/scala-library-" + ProjectScalaVersion() + ".jar")) with IsFile
  object ProjectScalaReflectJar extends Default(file("scala-libs/scala-reflect-" + ProjectScalaVersion() + ".jar")) with IsFile
  object ProjectScalaLibrarySourceJar extends Default(file("scala-libs/scala-library-" + ProjectScalaVersion() + "-sources.jar")) with IsFile
  object ProjectScalaCompilerJar extends Default(file("scala-libs/scala-compiler-" + MakerScalaVersion() + ".jar")) with IsFile
  object SbtInterfaceJar extends Default(file(MakerHome() + "/zinc-libs/com.typesafe.sbt-sbt-interface-" + DefaultSbtVersion + ".jar")) with IsFile
  object CompilerInterfaceSourcesJar extends Default(file(MakerHome() + "/zinc-libs/com.typesafe.sbt-compiler-interface-" + DefaultSbtVersion + "-sources.jar")) with IsFile

  object JavaSystemProperties extends IsOptionalFile {
    def properties = {
      val properties = new java.util.Properties()
        apply().foreach{file => properties.load(new FileInputStream(file))}
      properties
    }
    def asMap = {
      val ps = properties
      JavaConversions.asScalaSet(ps.stringPropertyNames).map{
        p => p -> ps.getProperty(p)
      }.toMap
    }
  }
  object ShowFailingTestException extends Default(false) with IsBoolean
  object CopyResourcesBeforeCompiling extends Default(false) with IsBoolean

  /** 
   * Set to true in maker.sh if we are executing a maker command,
   * rather than interacting with maker in the repl. 
   */
  object ExecMode extends SystemPropertyWithDefault("maker.execmode", false) with IsBoolean

  // Compilation will require large amounts of memory - tests hopefully less so
  private def defaultTestProcessMemory : Int = {
    val runtimeMemory = (Runtime.getRuntime.maxMemory / 1024 / 1024).toInt
    (runtimeMemory / 2) min 1024
  }
  object TestProcessMemoryInMB extends Default(defaultTestProcessMemory) with IsInt
  object NumberOfTaskThreads extends Default((Runtime.getRuntime.availableProcessors / 2 max 1) min 4) with IsInt
  object CompilationCache extends EmptyString

  /*
   * Switches off sundry output with testing maker
   */
  object RunningInMakerTest extends SystemPropertyWithDefault("maker.running.within.test", false) with IsBoolean

  // Show compiler output - normally switched off for tests
  object ShowCompilerOutput extends SystemPropertyWithDefault("show.compiler.output", true) with IsBoolean

  object LogCompilerClasspath extends SystemPropertyWithDefault("maker.show.compiler.output", false) with IsBoolean

  object LogCommands extends Default(true) with IsBoolean
  object LogCommandFile extends Default(file("maker-commands.log")) with IsFile

  object TmuxMessaging extends Default(true) with IsBoolean

  object ResourceCacheDirectory extends Default(file(System.getenv("HOME"), ".maker-resource-cache").makeDirs()) with IsFile

  object PublishLocalRootDir extends Default(file(System.getenv("HOME"), "updateOrCreate")) with IsFile


  def ++(moreOverrides : String*) = {
    val moreOverridesAsMap : Map[String, String] = moreOverrides.toList.grouped(2).map{
      case List(k, v) => k -> v
      case other => throw new Exception("Needs matching numbers of keys and values")
    }.toMap
    copy(overrides = overrides ++  moreOverridesAsMap)
  }

  def ++(rhs : MakerProps) = MakerProps(overrides ++ rhs.overrides)
  // DelayedInit should maker this unnecessary - scala bug?
  checkForInvalidProperties
}

object MakerProps {

  val DefaultScalaVersion = hackyReadVersion("scala_version")
  val DefaultSbtVersion = hackyReadVersion("sbt_version")

  private def hackyReadVersion(libname: String): String = {
    val source = io.Source.fromFile("external-resource-config")
    try {
      source.getLines().find(_.contains(libname)).map(_.split(" ")(2)).getOrElse{
        throw new RuntimeException("Unable to read version of " + libname)
      }
    } finally source.close()
  }

  def apply(file : File) : MakerProps = {
    new MakerProps(MMap() ++ propsFileToMap(file))
  }
  def apply(key1 : String, value1 : String, moreKeysAndValues : String*) : MakerProps = {
    MakerProps().++(key1 :: value1 :: moreKeysAndValues.toList : _*)
  }
  def apply() : MakerProps = apply(file("Maker.conf"))

  def propsFileToMap(file : File) : Map[String, String] = {
    val p = new Properties()
    if (file.exists) {
	    val fis = new FileInputStream(file)
      p.load(fis)
			fis.close
    }
    Map() ++ JavaConversions.mapAsScalaMap(p.asInstanceOf[java.util.Map[String,String]])
  }
}
