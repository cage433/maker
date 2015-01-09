package maker.task.tasks

import java.io.File
import maker.project.Module
import maker.task.{DefaultTaskResult, SingleModuleTask, TaskResult}
import maker.task.compile._
import maker.utils.FileUtils._
import maker.utils.{Stopwatch, Int}
import maker.utils.os.Command
import org.apache.commons.io.FileUtils
import org.apache.commons.io.filefilter.TrueFileFilter
import scala.collection.JavaConverters._
import scalaz.syntax.std.ToBooleanOps
import org.slf4j.LoggerFactory

case class PackageMainJarTask(module: Module) extends PackageJarTask(module) {
  def name = "Package Main Jar"
  def upstreamTasks = 
    SourceCompileTask(module) :: module.immediateUpstreamModules.map(PackageMainJarTask)

  protected def outputArtifact: File = module.outputArtifact
  protected def outputDir: File = module.compilePhase.outputDir
  protected def resourceDir: File = module.resourceDir
}

abstract class PackageJarTask(module: Module) extends SingleModuleTask(module) with ToBooleanOps {
  private val props = module.props
  private val logger = LoggerFactory.getLogger(this.getClass)

  def exec(results: Iterable[TaskResult], sw: Stopwatch) = synchronized {
    doPackage(results, sw)
  }

  protected def outputArtifact: File
  protected def outputDir: File
  protected def resourceDir: File


  private def doPackage(results: Iterable[TaskResult], sw: Stopwatch) = {
    val jar = props.Jar().getAbsolutePath
    def jarCommand(updateOrCreate: String, targetFile: File, baseDir: File) = {
      Command(
        List(jar, updateOrCreate, outputArtifact.getAbsolutePath, "-C", baseDir.getAbsolutePath, "."): _*
      )
    }

    def createJarCommand(baseDir: File) = jarCommand("cf", outputArtifact, baseDir)
    def updateJarCommand(baseDir: File) = jarCommand("uf", outputArtifact, baseDir)

    val cmds = createJarCommand(outputDir) :: (resourceDir.exists ? List(updateJarCommand(resourceDir)) | Nil)

    if (!module.packageDir.exists)
      module.packageDir.mkdirs

    cmds.find(_.exec != 0) match {
      case Some(failingCommand) =>
        DefaultTaskResult(this, false, sw, message = Some(failingCommand.savedOutput))
      case None =>
        DefaultTaskResult(this, true, sw)
    }
  }
}
