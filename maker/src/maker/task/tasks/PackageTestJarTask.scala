package maker.task.tasks

import maker.task.Task
import maker.project.Module
import maker.task.TaskResult
import maker.utils.Stopwatch
import maker.utils.os.Command
import maker.utils.FileUtils._
import java.io.File
import maker.task.compile.TestCompileTask

case class PackageTestJarTask(module : Module) extends Task {
  def name = "Package Jar"
  val props = module.props
  val log = props.log

  def upstreamTasks = TestCompileTask(module) :: module.immediateUpstreamModules.map(PackageTestJarTask)

  // Note: until we support scopes properly we have to be careful what we put on the runtime classpath
  //   and in package runtime binary artifacts (so test scope content is deliberately excluded here)
  private lazy val mainDirs : List[File] =  List(module.compilePhase.outputDir, module.resourceDir).filter(_.exists)

  private lazy val testDirs : List[File] =  List(module.testCompilePhase.outputDir, module.testResourceDir).filter(_.exists)


  def exec(results : Iterable[TaskResult], sw : Stopwatch) = {
    synchronized{
      if (fileIsLaterThan(module.testOutputArtifact, mainDirs) || fileIsLaterThan(module.testOutputArtifact, testDirs)) {
        log.info("Packaging up to date for " + module.name + ", skipping...")
        TaskResult.success(this, sw)
      } else {
        doPackage(results, sw)
      }
    }
  }

  private def doPackage(results : Iterable[TaskResult], sw : Stopwatch) = {
    val jar = props.Jar().getAbsolutePath

    case class WrappedCommand(cmd : Command, ignoreFailure : Boolean){
      def exec : Int = {
        (cmd.exec(), ignoreFailure) match {
          case  (0, _) ⇒ 0
          case (errorNo, true) ⇒ {
            log.warn("Ignoring  error in " + cmd + ". Artifact may be missing content")
            0
          }
          case (errorNo, _) ⇒ errorNo
        }
      }
    }
    def jarCommand(updateOrCreate : String,targetFile : File, dir : File) = WrappedCommand(
      Command(props, List(jar, updateOrCreate,targetFile.getAbsolutePath, "-C", dir.getAbsolutePath, "."): _*),
      ignoreFailure = false
    )

    def createTestJarCommand(dir : File) = jarCommand("cf",module.testOutputArtifact,dir)
    def updateTestJarCommand(dir : File) = jarCommand("uf",module.testOutputArtifact,dir)

    if (!module.packageDir.exists)
      module.packageDir.mkdirs

    val cmds : List[WrappedCommand] = createTestJarCommand(testDirs.head) :: testDirs.tail.map(updateTestJarCommand)

    cmds.find(_.exec != 0) match {
      case Some(failingCommand) ⇒ TaskResult.failure(this, sw, failingCommand.cmd.savedOutput)
      case None ⇒ TaskResult.success(this, sw)
    }
  }
}
