package maker

import scala.collection.mutable.{Map ⇒ MMap}
import java.io.File
import java.util.Properties
import scala.collection.JavaConversions
import maker.utils.MakerLog
import java.io.FileInputStream
import maker.utils.FileUtils._


case class MakerProps (overrides : MMap[String, String]) extends PropsTrait{
  
  lazy val log = {
    val log = MakerLog(MakerLogLevel())
    log
  }

  object HttpProxyHost extends IsOptionalString
  object HttpProxyPort extends IsOptionalString
  object HttpNonProxyHosts extends IsOptionalString

  object MakerHome extends SystemProperty("maker.home") with IsString
  object MakerTestReporterJar extends Default(MakerHome() + "/maker-scalatest-reporter.jar") with IsFile

  object MakerLogLevel extends SystemPropertyWithDefault("maker.log.level", "INFO") with IsLogLevel

  val httpProperties : List[(String, String)]= List(HttpProxyHost, HttpProxyPort, HttpNonProxyHosts).flatMap{
    case prop => List(prop.name).zip(prop())
  }

  object ScalaHome extends EnvProperty("SCALA_HOME") with IsFile
  object JavaHome extends EnvProperty("JAVA_HOME", "JDK_HOME") with IsFile
  object Java extends Default(JavaHome() + "/bin/java") with IsFile
  object Javac extends Default(JavaHome() + "/bin/javac") with IsFile
  object Jar extends Default(JavaHome() + "/bin/jar") with IsFile
  object ScalaVersion extends Default("2.9.2") with IsString
  object HomeDir extends SystemProperty("user.home") with IsFile
  object VimErrorFile extends Default("vim-compile-output") with IsFile
  object RemoteTaskPort extends SystemPropertyWithDefault("maker.remote.task.port", 10101) with IsInt
  object Organisation extends Default("The Acme Org") with IsString
  object GroupId extends Default("org.acme") with IsString
  object Version extends Default("1.0-SNAPSHOT") with IsString
  object DefaultPublishResolver extends Default("default") with IsString
  object UseZincCompiler extends Default(false) with IsBoolean

  /**
   * Maker has its own logback file which applies during compilation, 
   * this is the one that is used when running tests and main methods
   */
  object ProjectLogbackConfigFile extends Default(file("logback.xml")) with IsFile
  object ProjectTestLogbackConfigFile extends Default(file("logback-unit-tests.xml")) with IsFile

  object ScalaLibraryJar extends Default(file("scala-lib/scala-library-" + ScalaVersion() + ".jar")) with IsFile
  object ScalaCompilerJar extends Default(file("zinc-libs/scala-compiler-" + ScalaVersion() + ".jar")) with IsFile
  object SbtInterfaceJar extends Default(file("zinc-libs/sbt-interface-0.12.1.jar")) with IsFile
  object CompilerInterfaceSourcesJar extends Default(file("zinc-libs/compiler-interface-sources-0.12.1.jar")) with IsFile


  object PomTemplateFile extends IsOptionalFile
  object PomBuildTemplateFile extends IsOptionalFile

  object PomPluginRepo extends Default("""
    <pluginRepositories>
      <pluginRepository>
        <id>nexus</id>
        <url>http://nexus.myorg.com:8081/nexus/content/groups/myProj</url>
      </pluginRepository>
    </pluginRepositories>""") with IsXml

  object JavaSystemProperties extends IsOptionalFile {
    def properties = {
      val properties = new java.util.Properties()
        apply().foreach{file ⇒ properties.load(new FileInputStream(file))}
      properties
    }
    def asMap = {
      val ps = properties
      JavaConversions.asScalaSet(ps.stringPropertyNames).map{
        p ⇒ p → ps.getProperty(p)
      }.toMap
    }
  }
  object ScmUrl extends EmptyString
  object ScmConnection extends EmptyString
  object Licenses extends EmptyString
  object Developers extends EmptyString
  object Username extends EmptyString
  object Password extends EmptyString

  object IvyChecksums extends IsOptionalString

  /** 
  * Check for ivy updates before each compile - strictly a
  * dependency but does have a performance impact, hence
  * optional
  */
  object UpdateOnCompile extends Default(false) with IsBoolean

  /**
   * Think that large info objects may be causing a memory leak
   */
  object StripInfoFromTaskResults extends Default(true) with IsBoolean

  object ShowFailingTestException extends Default(false) with IsBoolean
  object CopyResourcesBeforeCompiling extends Default(false) with IsBoolean
  object FailCompilationIfNoClassesGenerated extends Default(true) with IsBoolean

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
  object NumberOfTaskThreads extends Default(Runtime.getRuntime.availableProcessors / 2 max 1) with IsInt
  object CompilationCache extends EmptyString

  object MinimumTaskReportingTime extends Default(100) with IsInt


  // Show compiler output - normally switched off for tests
  object ShowCompilerOutput extends SystemPropertyWithDefault("show.compiler.output", true) with IsBoolean

  object ShowTestProgress extends SystemPropertyWithDefault("maker.show.test.progress", false) with IsBoolean
  object LogCompilerClasspath extends SystemPropertyWithDefault("maker.show.compiler.output", false) with IsBoolean

  object LogCommands extends Default(true) with IsBoolean
  object LogCommandFile extends Default(file("maker-commands.log")) with IsFile

  def ++(moreOverrides : String*) = {
    val moreOverridesAsMap : Map[String, String] = moreOverrides.toList.grouped(2).map{
      case List(k, v) ⇒ k → v
      case other ⇒ throw new Exception("Needs matching numbers of keys and values")
    }.toMap
    copy(overrides = overrides ++  moreOverridesAsMap)
  }
}

object MakerProps {
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
      p.load(new FileInputStream(file))
    }
    Map() ++ JavaConversions.mapAsScalaMap(p.asInstanceOf[java.util.Map[String,String]])
  }
}

