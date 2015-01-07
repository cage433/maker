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
import ch.qos.logback.classic.Logger
import org.slf4j.LoggerFactory

case class PackageMainJarTask(module: Module) extends PackageJarTask(module) {
  def name = "Package Main Jar"
  def upstreamTasks = SourceCompileTask(module) :: module.immediateUpstreamModules.map(PackageMainJarTask)

  protected def outputArtifact: File = module.outputArtifact
  protected def outputDir: File = module.compilePhase.outputDir
  protected def resourceDir: File = module.resourceDir
  protected def managedResources: List[File] = allManagedResources.filter(module.includeInMainJar)
}

abstract class PackageJarTask(module: Module) extends SingleModuleTask(module) {
  private val props = module.props
  private val logger = LoggerFactory.getLogger(this.getClass)

  def exec(results: Iterable[TaskResult], sw: Stopwatch) = synchronized {
    doPackage(results, sw)
  }

  protected def outputArtifact: File
  protected def outputDir: File
  protected def resourceDir: File
  protected def managedResources: List[File]

  protected def allManagedResources: List[File] =
    if (!module.managedResourceDir.exists) Nil
    else FileUtils.iterateFiles(
      module.managedResourceDir, TrueFileFilter.INSTANCE, TrueFileFilter.INSTANCE
    ).asScala.filter(_.isFile).toList

  private def doPackage(results: Iterable[TaskResult], sw: Stopwatch) = {
    val jar = props.Jar().getAbsolutePath
    def jarCommand(updateOrCreate: String, targetFile: File, baseDir: File, file: File) = {
      val relativeName = if (file == baseDir) "." else file.relativeTo(baseDir).toString
      Command(
        List(jar, updateOrCreate, outputArtifact.getAbsolutePath, "-C", baseDir.getAbsolutePath, relativeName): _*
      )
    }

    def createJarCommand(baseDir: File, file: File) = jarCommand("cf", outputArtifact, baseDir, file)
    def updateJarCommand(baseDir: File, file: File) = jarCommand("uf", outputArtifact, baseDir, file)

    val cmds: List[Command] = createJarCommand(outputDir, outputDir) :: {
      (resourceDir, resourceDir) ::
      managedResources.map((module.managedResourceDir, _))
    }.collect {
      case (base, file) if (file.exists) => updateJarCommand(base, file)
    }

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
