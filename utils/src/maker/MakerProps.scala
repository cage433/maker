package maker

import scala.collection.mutable.{Map => MMap}
import java.io.File
import java.util.Properties
import scala.collection.JavaConversions
import maker.utils.MakerLog
import java.io.FileInputStream
import maker.utils.FileUtils._


case class MakerProps (private val root_ : File, overrides : MMap[String, String]) extends PropsTrait{

  val root = root_.asAbsoluteFile
  
  lazy val log = {
    val log = MakerLog()
    log
  }

  //object MakerTestReporterClasspath extends SystemPropertyWithDefault("maker.test.reporter.classpath", file(root, "maker-scalatest-reporter.jar")) with IsString
  object MakerTestReporterClasspath extends SystemPropertyWithDefault("maker.test.reporter.classpath", file(root, "test-reporter/target-maker/classes/")) with IsFile
  object ScalaHome extends EnvProperty("SCALA_HOME") with IsFile
  object JavaHome extends EnvProperty("JAVA_HOME", "JDK_HOME") with IsFile
  object Java extends Default(JavaHome() + "/bin/java") with IsFile
  object Javac extends Default(JavaHome() + "/bin/javac") with IsFile
  object Jar extends Default(JavaHome() + "/bin/jar") with IsFile
  object MakerScalaVersion extends Default("2.9.2") with IsString
  object ProjectScalaVersion extends Default("2.9.2") with IsString
  object VimErrorFile extends Default("vim-compile-output") with IsFile
  object GroupId extends Property with IsString
  object Compiler extends Default("scalac") with IsString
  object ResolversFile extends Default(file(root, "resource-resolvers")) with IsFile
  object VersionsFile extends Default(file(root, "resource-versions")) with IsFile
  object IvySettingsFile extends Default(file(root, "ivysettings.xml")) with IsFile
  def resourceVersions() = MakerProps.propsFileToMap(VersionsFile())
  def resourceResolvers() : Map[String, String] = MakerProps.propsFileToMap(ResolversFile())
  def defaultResolver() : String = resourceResolvers.getOrElse("default", throw new RuntimeException("No default resolver"))

  /**
   * Maker has its own logback file which applies during compilation, 
   * this is the one that is used when running tests and main methods
   */
  object LogbackTestConfigFile extends SystemPropertyWithDefault("maker.test.logback.config", file(root, "logback-unit-tests.xml")) with IsFile

  object ProjectScalaLibraryJar extends Default(file(root, "scala-libs/scala-library-" + ProjectScalaVersion() + ".jar")) with IsFile
  object ProjectScalaLibrarySourceJar extends Default(file(root, "scala-libs/scala-library-" + ProjectScalaVersion() + "-sources.jar")) with IsFile
  object ProjectScalaCompilerJar extends Default(file(root, "scala-libs/scala-compiler-" + MakerScalaVersion() + ".jar")) with IsFile
  object SbtInterfaceJar extends Default(file(root, "zinc-libs/com.typesafe.sbt-sbt-interface-0.12.1.jar")) with IsFile
  object CompilerInterfaceSourcesJar extends Default(file(root, "zinc-libs/com.typesafe.sbt-compiler-interface-0.12.1-sources.jar")) with IsFile

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


  // Show compiler output - normally switched off for tests
  object ShowCompilerOutput extends SystemPropertyWithDefault("show.compiler.output", true) with IsBoolean

  object LogCompilerClasspath extends SystemPropertyWithDefault("maker.show.compiler.output", false) with IsBoolean

  object LogCommands extends Default(true) with IsBoolean
  object LogCommandFile extends Default(file(root, "maker-commands.log")) with IsFile

  object TmuxMessaging extends Default(true) with IsBoolean

  object ResourceCacheDirectory extends Default(file(System.getenv("HOME"), ".maker-resource-cache").makeDirs()) with IsFile

  object PublishLocalRootDir extends Default(file(System.getenv("HOME"), ".maker-publish-local")) with IsFile

  object TestReporter extends Default("maker.scalatest.MakerTestReporter") with IsString
  
  object RunningInUserMode extends Default(true) with IsBoolean

  def ++(moreOverrides : String*) = {
    val moreOverridesAsMap : Map[String, String] = moreOverrides.toList.grouped(2).map{
      case List(k, v) => k -> v
      case other => throw new Exception("Needs matching numbers of keys and values")
    }.toMap
    copy(overrides = overrides ++  moreOverridesAsMap)
  }

  def ++(rhs : MakerProps) = MakerProps(root, overrides ++ rhs.overrides)
  // DelayedInit should maker this unnecessary - scala bug?
  checkForInvalidProperties

  def initialiseTestProps(root : File, moreProps : String*) : MakerProps = {
    val makerDotConf = file(root, "Maker.conf")
    def writeProperty(key : String, value : String){
      appendToFile(makerDotConf, key + "=" + value + "\n")
    }
    writeProperty("ShowCompilerOutput", "false")
    writeProperty("GroupId", "MakerTestGroupID")
    writeProperty("TmuxMessaging", "false")
    writeProperty("RunningInUserMode", "false")
    writeProperty("PublishLocalRootDir", file(root, ".maker-publish-local").makeDirs().absPath)
    List(
      ProjectScalaLibraryJar, 
      ProjectScalaLibrarySourceJar, 
      ProjectScalaCompilerJar, 
      SbtInterfaceJar, 
      CompilerInterfaceSourcesJar, 
      ResolversFile, 
      VersionsFile,
      MakerTestReporterClasspath,
      LogbackTestConfigFile,
      LogCommandFile,
      IvySettingsFile
    ).foreach{
      prop => 
        writeProperty(prop.name, prop().absPath)
    }
    moreProps.toList.grouped(2).foreach{
      case List(key, value) => writeProperty(key, value)
      case _ => throw new RuntimeException("Need even number for key/value pairs")
    }
    MakerProps(makerDotConf)
  }
}

object MakerProps {
  def apply(file : File, moreKeysAndValues : String*) : MakerProps = {
    val (root, propsMap) : (File, MMap[String, String]) = if (file.isDirectory) 
      (file, MMap.empty) 
    else 
      (file.getParentFile, MMap() ++ propsFileToMap(file))
    MakerProps(root, propsMap) ++ (moreKeysAndValues.toList :_*)
  }

  def propsFileToMap(file : File) : Map[String, String] = {
    val p = new Properties()
    if (file.exists) {
	    val fis = new FileInputStream(file)
      p.load(fis)
			fis.close
    }
    Map() ++ JavaConversions.mapAsScalaMap(p.asInstanceOf[java.util.Map[String,String]])
  }

  def initialiseTestProps(root : File, moreProps : String*) : MakerProps = {
    MakerProps(file(".").asAbsoluteFile).initialiseTestProps(root, moreProps : _*)
  }
}

