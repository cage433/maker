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
  def upstreamTasks : List[Task] = List(PackageJarTask(baseProject, modules, SourceCompilePhase, Some(version)))

  def exec(results : Iterable[TaskResult], sw : Stopwatch) = {
    IvyLock.synchronized{
      println("Debug: " + (new java.util.Date()) + "publish local ") 
      val result = doPublish(baseProject, results, sw)
      println("Debug: " + (new java.util.Date()) + "finish publish local ") 
      result
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
  
    baseProject.publishLocalDir(version).deleteAll()
    def versionedFilename(file : File) : String = {
     val basename :: extension :: Nil = file.basename.split('.').toList
     s"$basename-$version.$extension"
    }
    // TODO - fix the includeUpstreamModules hack
    FileUtils.writeToFile(baseProject.publishLocalPomFile(version), PomUtils.pomXmlText(baseProject, version, includeUpstreamModules = modules.size > 1))
    var result = true
    if (signArtifacts)
      result = signFile(baseProject.publishLocalPomFile(version))

    result &&= Vector(
      (baseProject.packageJar(SourceCompilePhase, Some(version)), s"${baseProject.name}-$version.jar"),
      (baseProject.sourcePackageJar(SourceCompilePhase, Some(version)), s"${baseProject.name}-$version-sources.jar"),
      (baseProject.docPackageJar, s"${baseProject.name}-$version-javadoc.jar")
    ).filter(_._1.exists).forall{
      case (jar, versionedBasename) => 
        val fileWithVersion = file(baseProject.publishLocalJarDir(version), versionedBasename)
        fileWithVersion.delete
        try {
          copyFile(jar, fileWithVersion)
        } catch {
          case e : Exception => 
            println(e)
            throw e
        }
        if (signArtifacts)
          signFile(fileWithVersion)
        else
          true
    }
    DefaultTaskResult(this, result, sw)
  }
}
