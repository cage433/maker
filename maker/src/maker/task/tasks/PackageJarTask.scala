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

case class PackageJarTask(module: Module, compilePhase : CompilePhase) extends SingleModuleTask(module) with ToBooleanOps {
  def name = compilePhase match {
    case SourceCompilePhase => "Package Main Jar"
    case TestCompilePhase => "Package Test Jar"
  }
  def upstreamTasks = 
    CompileTask(module, compilePhase) :: module.immediateUpstreamModules.map(PackageJarTask(_, compilePhase))

  def exec(results: Iterable[TaskResult], sw: Stopwatch) = synchronized {
    doPackage(results, sw)
  }

  val List(outputArtifact, outputDir, resourceDir) = compilePhase match {
    case SourceCompilePhase => 
      List(module.outputArtifact, module.compilePhase.outputDir, module.resourceDir)
    case TestCompilePhase =>
      List(module.testOutputArtifact, module.testCompilePhase.outputDir, module.testResourceDir)
  }


  private def doPackage(results: Iterable[TaskResult], sw: Stopwatch) = {
    val jar = module.props.Jar().getAbsolutePath
    def jarCommand(updateOrCreate: String, baseDir: File) = {
      Command(
        List(jar, updateOrCreate, outputArtifact.getAbsolutePath, "-C", baseDir.getAbsolutePath, "."): _*
      )
    }

    val cmds = jarCommand("cf", outputDir) :: (resourceDir.exists ? List(jarCommand("uf", resourceDir)) | Nil)

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
