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
case class CreateDeployTask(project: Project, buildTests: Boolean, version: Option[String] = None) extends Task {
  def baseProject = project

  private val logger = LoggerFactory.getLogger(this.getClass)
  val baseOutputDir = file(project.rootAbsoluteFile, "/target-maker/deploy/")
  val jarsDir = file(baseOutputDir, "/jars/")
  val thirdPartyJars = file(baseOutputDir, "/thirdpartyjars/")
  val binDir = file(baseOutputDir, "/bin/")

  def name = "Create Deploy"

  def module = project
  def upstreamTasks = project.allUpstreamModules.map(PackageJarTask(_, SourceCompilePhase)) ::: {
    if (buildTests) project.allUpstreamModules.map(PackageJarTask(_, TestCompilePhase))
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

    copyDirectoryAndPreserve(file(project.rootAbsoluteFile, "/bin/"), binDir)

    val appJars = project.allUpstreamModules map { m =>
      val out = file(jarsDir, m.outputArtifact.getName)
      copyFile(m.outputArtifact, out)
      out.relativeTo(baseOutputDir).getPath
    }

    val tpJars = for {
      m <- project.allModules
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
      val testJars = project.allUpstreamModules map { m =>
        val out = file(testJarsDir, m.testOutputArtifact.getName)
        copyFile(m.testOutputArtifact, out)
        out.relativeTo(baseOutputDir).getPath
      }

      val testClasspathString = (appJars ::: tpJars ::: testJars).distinct.mkString(":")
      writeToFile(file(baseOutputDir, "/bin/deploy-test-classpath.sh"), "export CLASSPATH=" + testClasspathString)
      version.foreach(v => writeToFile(file(project.rootAbsoluteFile, "/version.txt"), v))
    }
  }
}
