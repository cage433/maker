package maker

import scala.collection.mutable.{Map => MMap}
import java.io.{File, FileInputStream}
import java.util.Properties
import scala.collection.JavaConversions
import maker.utils.{FileUtils, Int}
import maker.utils.FileUtils._
import ch.qos.logback.classic.Logger
import org.slf4j.LoggerFactory


case class MakerProps (overrides : MMap[String, String]) extends PropsTrait{
  
  import MakerProps._
  object JavaHome extends EnvProperty("JAVA_HOME", "JDK_HOME") with IsFile
  object Java extends Default(JavaHome() + "/bin/java") with IsFile

  /**
   * The debug files should contain a single number, indicating the port to use for remote debugging.
   */
  object DebugPortMain extends Default(file("DEBUG-PORT-MAIN")) with IsFile
  object DebugPortTest extends Default(file("DEBUG-PORT-TEST")) with IsFile

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

  object GPG_PassPhrase extends EnvProperty("MAKER_GPG_PASS_PHRASE") with IsString
  object SonatypeCredentials extends EnvProperty("MAKER_SONATYPE_CREDENTIALS") with IsString
  object HttpProxy extends EnvProperty("MAKER_HTTP_PROXY") with IsOptionalString

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
