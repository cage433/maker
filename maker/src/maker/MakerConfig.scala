package maker

import maker.utils.FileUtils._
import java.io.File
import maker.utils.Int
import org.eclipse.aether.util.artifact.JavaScopes
import maker.project.DependencyPimps

trait MakerConfig extends DependencyPimps {

  private def maybeProperty(name: String): Option[String] = Option(System.getProperty(name))
  private def maybeEnvVar(name: String): Option[String] = Option(System.getenv(name))

  def runningInExecMode = {
    maybeProperty("maker.exec-mode").map(_.toBoolean).getOrElse(false)
  }

  // if debug port set then enable remote degubbing
  def remoteDebuggingOption = {
    maybeProperty("maker.debug.port") match {
      case Some(port) => 
        Seq(s"-agentlib:jdwp=transport=dt_socket,server=y,suspend=y,address=$port")
      case None => 
        Nil
    }
  }

  protected def taskThreadPoolSize: Option[Int] = None

  def unitTestHeapSize : Int = {
    maybeProperty("maker.unit-test-heap-size") match {
      case Some(size) => size.toInt
      case None => 
        val runtimeMemory = (Runtime.getRuntime.maxMemory / 1024 / 1024).toInt
        (runtimeMemory / 2) min 1024
    }
  }

  def javaHome = {
    maybeEnvVar("JAVA_HOME") orElse maybeEnvVar("JDK_HOME") match {
      case Some(dir) => file(dir)
      case None => 
        throw new IllegalStateException("JAVA_HOME or JDK_HOME must be specified")
    }
  }
  
  def javaExecutable = {
    file(javaHome, "bin", "java")
  }


  def httpResolvers = Seq(
    ("maven",  "http://repo1.maven.org/maven2/"),
    ("typesafe", "http://repo.typesafe.com/typesafe/releases/"),
    ("sonatype-snapshots", "https://oss.sonatype.org/content/repositories/snapshots/"),
    ("sonatype-releases", "https://oss.sonatype.org/content/repositories/releases/")
  )

  def gpgPassPhrase = maybeEnvVar("MAKER_GPG_PASS_PHRASE") match {
    case Some(phrase) => phrase
    case None => throw new Exception(s"MAKER_GPG_PASS_PHRASE variable not set")
  }
  def sonatypeCredentials = maybeEnvVar("MAKER_SONATYPE_CREDENTIALS") match {
    case Some(cred) => cred.split(":")
    case None => throw new Exception(s"MAKER_SONATYPE_CREDENTIALS variable not set")
  }

  def scalatestOutputParameters : String = "-oHL"

  /* Methods that are overriden by maker unit tests projects/modules */
  def reportBuildResult : Boolean = true

  def systemExitOnExecModeFailures : Boolean = true

  def updateIncludesSourceJars : Boolean = true
}
