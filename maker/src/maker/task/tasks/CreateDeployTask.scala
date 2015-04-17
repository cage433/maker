package maker.task.tasks

import maker.project._
import maker.task._
import maker.utils.FileUtils._
import maker.utils.Stopwatch
import maker.utils.maven.IvyLock
import org.apache.commons.io.FileUtils._
import org.slf4j.LoggerFactory
import scala.collection.immutable.Nil
import maker.task.compile.{SourceCompilePhase, TestCompilePhase}


/**
 * Creates jars that are ready for deployment.
 */
case class CreateDeployTask(baseProject: Project, buildTests: Boolean, version: Option[String] = None) extends Task {

  private lazy val logger = LoggerFactory.getLogger(this.getClass)
  val baseOutputDir = file(baseProject.rootAbsoluteFile, "/target-maker/deploy/")
  val jarsDir = file(baseOutputDir, "/jars/")
  val thirdPartyJars = file(baseOutputDir, "/thirdpartyjars/")
  val binDir = file(baseOutputDir, "/bin/")

  def name = "Create Deploy"

  def module = baseProject
  def upstreamTasks = baseProject.allUpstreamModules.map{m => PackageJarTask(m, Vector(m), SourceCompilePhase, version)} ::: {
    if (buildTests) baseProject.allUpstreamModules.map{m => PackageJarTask(m, Vector(m), TestCompilePhase, version)}
    else Nil
  }

  def exec(results: Iterable[TaskResult], sw: Stopwatch) = {
    println("Running CreateDeployTask")
    IvyLock.synchronized {
      doPublish()
    }
    DefaultTaskResult(this, true, sw)
  }

  protected def doPublish(): Unit = {
    logger.info("Creating deployment directory: " + baseOutputDir)
    baseOutputDir.deleteAll()
    baseOutputDir.mkdirs()

    copyDirectoryAndPreserve(file(baseProject.rootAbsoluteFile, "/bin/"), binDir)

    val appJars = baseProject.allUpstreamModules map { m =>
      val out = file(jarsDir, m.packageJar(SourceCompilePhase, version).getName)
      copyFile(m.packageJar(SourceCompilePhase, version), out)
      out.relativeTo(baseOutputDir).getPath
    }

    val tpJars = for {
      m <- baseProject.allModules
      j <- m.classpathJars
    } yield {
      val out = file(thirdPartyJars, "/" + j.getName)
      copyFile(j, out)
      out.relativeTo(baseOutputDir).getPath
    }

    val classpathString = (appJars ::: tpJars).distinct.mkString(":")
    writeToFile(file(baseOutputDir, "/bin/deploy-classpath.sh"), "export CLASSPATH=" + classpathString)

    if (buildTests) {
      val testJarsDir = file(baseOutputDir, "/testjars/")
      testJarsDir.mkdirs()
      val testJars = baseProject.allUpstreamModules map { m =>
        val out = file(testJarsDir, m.packageJar(TestCompilePhase, version).getName)
        copyFile(m.packageJar(TestCompilePhase, version), out)
        out.relativeTo(baseOutputDir).getPath
      }

      val testClasspathString = (appJars ::: tpJars ::: testJars).distinct.mkString(":")
      writeToFile(file(baseOutputDir, "/bin/deploy-test-classpath.sh"), "export CLASSPATH=" + testClasspathString)
      version.foreach(v => writeToFile(file(baseProject.rootAbsoluteFile, "/version.txt"), v))
    }
  }
}
