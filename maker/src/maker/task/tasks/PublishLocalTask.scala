package maker.task.tasks

import org.apache.commons.io.FileUtils._
import maker.project._
import maker.task._
import maker.utils.{Stopwatch, FileUtils}
import maker.utils.maven.IvyLock
import maker.PomUtils
import maker.task.compile.SourceCompilePhase
import java.io.File
import maker.utils.os.Command
import org.scalatest.Failed
import org.slf4j.LoggerFactory
import maker.utils.FileUtils._

case class PublishLocalTask(
  baseProject : BaseProject, 
  modules : Seq[Module],
  version : String,
  signArtifacts : Boolean
) extends Task {
  def name = "Publish Local"

  def baseProjects = Vector(baseProject)
  val logger = LoggerFactory.getLogger(this.getClass)
  def module = baseProject
  def upstreamTasks : List[Task] = List(PackageJarTask(baseProject, modules, SourceCompilePhase))

  def exec(results : Iterable[TaskResult], sw : Stopwatch) = {
    IvyLock.synchronized{
      doPublish(baseProject, results, sw)
    }
  }
  
  private def signFile(file : File) = {
    val signatureFile = new File(file.getAbsolutePath + ".asc")
    if (signatureFile.exists)
      signatureFile.delete
    val cmd = Command("gpg", "-ab", "--passphrase", module.props.GPG_PassPhrase(), file.getAbsolutePath)
    val result = cmd.exec 
    if (result != 0)
      logger.error("Failed to sign " + file)
    result == 0
  }

  private def doPublish(baseProject: BaseProject, results : Iterable[TaskResult], sw : Stopwatch) = {
  
    FileUtils.writeToFile(baseProject.publishLocalPomFile, PomUtils.pomXml(baseProject, version))
    var result = true
    if (signArtifacts)
      result = signFile(baseProject.publishLocalPomFile)

    result &&= Vector(
      baseProject.packageJar(SourceCompilePhase), 
      baseProject.sourcePackageJar(SourceCompilePhase), 
      baseProject.docPackageJar
    ).filter(_.exists).forall{
      jar => 
        copyFileToDirectory(jar, baseProject.publishLocalJarDir)
        if (signArtifacts)
          signFile(file(baseProject.publishLocalJarDir, jar.basename))
        else
          true
    }
    DefaultTaskResult(this, result, sw)
  }
}
