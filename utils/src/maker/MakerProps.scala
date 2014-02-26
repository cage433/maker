package maker

import scala.collection.mutable.{Map => MMap}
import java.io.File
import java.util.Properties
import scala.collection.JavaConversions
import maker.utils.MakerLog
import java.io.FileInputStream
import maker.utils.FileUtils._
import maker.utils.Implicits.RichString._
import ch.qos.logback.classic.Level
import scala.xml.{XML, NodeSeq}
import scala.util.{Properties => ScalaProps}


case class MakerProps (private val root_ : File, overrides : MMap[String, String] = MMap.empty) extends PropsTrait{

  val root = root_.asAbsoluteFile
  
  lazy val log = {
    val log = MakerLog()
    log
  }

  object ScalaHome extends EnvProperty("SCALA_HOME") with IsFile
  object JavaHome extends EnvProperty("JAVA_HOME", "JDK_HOME") with IsFile
  val Java = file(JavaHome() + "/bin/java").absPath
  val Javac = file(JavaHome() + "/bin/javac").absPath
  val Jar = file(JavaHome() + "/bin/jar").absPath
  object MakerScalaVersion extends Default("2.9.2") with IsString
  object ProjectScalaVersion extends Default("2.9.2") with IsString
  object VimErrorFile extends Default("vim-compile-output") with IsFile
  object GroupId extends Property with IsString
  object Compiler extends Default("scalac") with IsString
  object ResourceConfigFile extends Default(file(root, "maker-resource-config")) with IsFile
  object IvySettingsFile extends Default(file(root, "ivysettings.xml")) with IsFile
  def resourceVersions() = MakerProps.ResourceSettings(ResourceConfigFile()).versions
  def resourceResolvers() : Map[String, String] = MakerProps.ResourceSettings(ResourceConfigFile()).resolvers
  def defaultResolver() : String = resourceResolvers.getOrElse("default", throw new RuntimeException("No default resolver"))

  object ProjectScalaLibraryJar extends Default(file(root, "scala-libs/scala-library-" + ProjectScalaVersion() + ".jar")) with IsFile
  object ProjectScalaLibrarySourceJar extends Default(file(root, "scala-libs/scala-library-" + ProjectScalaVersion() + "-sources.jar")) with IsFile
  object ProjectScalaCompilerJar extends Default(file(root, "scala-libs/scala-compiler-" + MakerScalaVersion() + ".jar")) with IsFile
  object ProjectScalaCompilerSourceJar extends Default(file(root, "scala-libs/scala-compiler-" + MakerScalaVersion() + "-sources.jar")) with IsFile
  object ProjectScalaReflectJar extends Default(file(root, "scala-libs/scala-reflect-" + MakerScalaVersion() + ".jar")) with IsFile
  object ProjectScalaReflectSourceJar extends Default(file(root, "scala-libs/scala-reflect-" + MakerScalaVersion() + "-sources.jar")) with IsFile
  object ProjectJlineJar extends Default(file(root, "scala-libs/jline-" + MakerScalaVersion() + ".jar")) with IsFile
  object ProjectJlineSourceJar extends Default(file(root, "scala-libs/jline-" + MakerScalaVersion() + "-sources.jar")) with IsFile

  //def scalaLibs() = findJars(file(ScalaHome(), "lib"))
  def compilerJars() = List(ProjectScalaCompilerJar(), ProjectScalaLibraryJar())
    //ProjectScalaReflectJar(), ProjectJlineJar())

  def extraJars() = List( 
    //ProjectScalaReflectJar(), 
    //ProjectScalaActorsJar(), 
    ProjectJlineJar())
  

  object SbtInterfaceJar extends Default(file(root, "zinc-libs/com.typesafe.sbt-sbt-interface-0.12.4.jar")) with IsFile
  object CompilerInterfaceSourcesJar extends Default(file(root, "zinc-libs/com.typesafe.sbt-compiler-interface-0.12.4-sources.jar")) with IsFile

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
  //object NumberOfTaskThreads extends Default((Runtime.getRuntime.availableProcessors / 2 max 1) min 4) with IsInt
  object NumberOfTaskThreads extends Default(1) with IsInt
  object CompilationCache extends EmptyString

  object LogCommands extends Default(true) with IsBoolean
  object LogCommandFile extends Default(file(root, "maker-commands.log")) with IsFile

  object TmuxMessaging extends Default(true) with IsBoolean

  object ResourceCacheDirectory extends Default(file(System.getenv("HOME"), ".maker-resource-cache").makeDirs()) with IsFile
  object PublishLocalRootDir extends Default(file(System.getenv("HOME"), ".maker-publish-local")) with IsFile

  /* 
   * Switches off sundry output with testing maker
   */
  object RunningInMakerTest extends SystemPropertyWithDefault("maker.running.within.test", false) with IsBoolean

  object StopCompileOutput extends Default(RunningInMakerTest()) with IsBoolean
  object ContinuousTaskWaitInMillis extends Default(50) with IsInt

  object MakerTestReporterClasspath extends SystemProperty("maker.test.reporter.classpath") with IsFile

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

 def initialiseTestProps(root : File, cwdProps : MakerProps = MakerProps(file(".").asAbsoluteFile)) : MakerProps = {
    val makerDotConf = file(root, "Maker.conf")
    def writeProperty(key : String, value : String){
      appendToFile(makerDotConf, key + "=" + value + "\n")
    }
    writeProperty("GroupId", "MakerTestGroupID")
    writeProperty("TmuxMessaging", "false")
    writeProperty("RunningInMakerTest", "true")
    writeProperty("PublishLocalRootDir", file(root, ".maker-publish-local").makeDirs().absPath)
    List(
      cwdProps.ProjectScalaLibraryJar,
      cwdProps.ProjectScalaLibrarySourceJar,
      cwdProps.ProjectScalaCompilerJar,
      cwdProps.ProjectScalaCompilerSourceJar,
      cwdProps.ProjectScalaReflectJar,
      cwdProps.ProjectScalaReflectSourceJar,
      cwdProps.ProjectJlineJar,
      cwdProps.ProjectJlineSourceJar,
      cwdProps.SbtInterfaceJar,
      cwdProps.CompilerInterfaceSourcesJar,
      cwdProps.ResourceConfigFile,
      cwdProps.LogCommandFile,
      cwdProps.IvySettingsFile,
      cwdProps.MakerTestReporterClasspath
    ).foreach{
      prop => 
        writeProperty(prop.name, prop().absPath)
    }
    MakerProps(makerDotConf)
  }

  case class ResourceSettings(resolvers : Map[String, String], versions : Map[String, String])
  object ResourceSettings{
    def apply(settingsFile : File) : ResourceSettings = {
      def buildMap(key : String) : Map[String, String] = {
        settingsFile.readLines.filter(_.startsWith(key)).map{
          line => 
            val List(x, y) = line.split(" ").toList.filterNot(_ == "").tail
            x -> y
        }.toMap
      }
      ResourceSettings(
        buildMap("resolver:"),
        buildMap("version:")
      )
    }
  }
}

class PropertyNotSetException(key : String) extends Throwable("Property " + key +  " not set")

/**
  * In a trait, rather than just in Props, as it allows 
  * the different types of properties to be tested 
  */
trait PropsTrait extends DelayedInit{
  protected def overrides : MMap[String, String]

  protected def checkForInvalidProperties{
    overrides.foreach{
      case (o, _) => 
      assert(propertyMethods.map(_.getName).toSet.contains(o), "Overiding non existant property " + o)
    }
  }
  /**
   DelayedInit ensures overrides are constructed before 
   their values are checked
  */
  def delayedInit(x : => Unit){
    x
    checkForInvalidProperties
  }
  val propertyMethods = this.getClass.getMethods.filter{
    m =>
      classOf[Property].isAssignableFrom(m.getReturnType) && m.getParameterTypes.isEmpty
  }

  override def toString = {
    val buffer = new StringBuffer
    propertyMethods.foreach(m =>
        try {
          buffer.append(m.invoke(this) + "\n")
        } catch {
          case t: Throwable => 
            buffer.append(m.getName + " threw " + t + "\n")
        }
      )
    buffer.toString
  }



  trait Property{
    type T
    def stringValue : String = overrides.getOrElse(name, throw new PropertyNotSetException(name))
    def apply() : T 
    def name = {
      val objectName = getClass.getName
      objectName.split('$').last
    }
    override def toString = {
      val valueAsString = try {
        apply().toString
      } catch {
        case _ : PropertyNotSetException => "Property no set"
      }
      name + "=" + valueAsString
    }
    def := (newValue : String){
      overrides += (name -> newValue)
    }
    def := (newValue : T){
      overrides += (name -> newValue.toString)
    }
  }

  abstract class Default(default : => Any) extends Property{
    override def stringValue = overrides.getOrElse(name, default.toString)
  }

  abstract class SystemProperty(key : String) extends Property{
    protected def systemValue = ScalaProps.propOrNone(key)
    override def stringValue = overrides.getOrElse(name, systemValue.getOrElse{throw new Exception("Required System property " + name + " not set")})
    def toCommandLine(value : String) = "-D%s=%s" % (key, value)
    def toCommandLine = "-D%s=%s" % (key, apply())
    def toCommandLine(appender : T => String) = "-D%s=%s" % (key, appender(apply()))
  }

  abstract class SystemPropertyWithDefault(key : String, default : Any) extends SystemProperty(key){
    override def stringValue = {
      overrides.getOrElse(name, systemValue.getOrElse(default.toString))
    }
  }

  trait IsString{
    self: Property => 
    type T = String
    def apply() = self.stringValue
  }

  trait IsFile{
    self: Property => 
    type T = File
    def apply() = file(self.stringValue)
  }


  trait IsBoolean{
    self: Property => 
    type T = Boolean
    def apply() = java.lang.Boolean.parseBoolean(stringValue)
  }

  trait IsXml{
    self: Property =>
    type T = NodeSeq
    def apply() = XML.loadString(self.stringValue)
  }

  trait IsOptionalString extends Property{
    type T = Option[String]
    override def stringValue = throw new UnsupportedOperationException()
    def apply() = overrides.get(name)
  }
  trait IsOptionalFile extends Property{
    type T = Option[File]
    override def stringValue = throw new UnsupportedOperationException()
    def apply() = overrides.get(name).map(file)
  }
  trait IsInt{
    self: Property => 
    type T = Int
    def apply() = self.stringValue.toInt
  }
  trait IsLogLevel{
    self: Property => 
    type T = Level
    def apply() = Level.toLevel(stringValue)
  }

  class EmptyString extends Default("") with IsString
  
  abstract class EnvProperty(vars : String*) extends Property{
    override def stringValue = vars.toList.flatMap{
      v => Option(System.getenv(v))
    }.headOption.getOrElse{throw new PropertyNotSetException(vars.toList.mkString(","))}
  }
}
