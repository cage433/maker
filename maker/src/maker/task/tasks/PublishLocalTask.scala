package maker.task.tasks

import org.apache.commons.io.FileUtils._
import maker.project._
import maker.task._
import maker.utils.{Stopwatch, FileUtils}
import maker.utils.maven.IvyLock
import maker.{PomUtils, ConfigPimps, ScalaVersion}
import maker.task.compile.SourceCompilePhase
import java.io.File
import maker.utils.os.Command
import org.scalatest.Failed
import org.slf4j.LoggerFactory
import maker.utils.FileUtils._

case class PublishLocalTask(
  project : Project, 
  version : String,
  signArtifacts : Boolean,
  scalaVersion : ScalaVersion
) 
  extends Task 
  with ConfigPimps
{
  import project.config
  def name = "Publish Local"

  def upstreamTasks : List[Task] = List(PackageJarTask(project, Some(version), scalaVersion))

  def exec(results : Iterable[TaskResult], sw : Stopwatch) = {
    IvyLock.synchronized{
      doPublish(project, results, sw)
    }
  }
  
  private def signFile(file : File) = {
    val signatureFile = new File(file.getAbsolutePath + ".asc")
    if (signatureFile.exists)
      signatureFile.delete
    val cmd = Command("gpg", "-ab", "--passphrase", config.gpgPassPhrase, file.getAbsolutePath)
    val result = cmd.run
    if (result != 0)
      logger.error("Failed to sign " + file)
    result == 0
  }

  private def doPublish(project: Project, results : Iterable[TaskResult], sw : Stopwatch) = {
  
    project.publishLocalDir(version, scalaVersion).deleteAll()
    def versionedFilename(file : File) : String = {
     val basename :: extension :: Nil = file.basename.split('.').toList
     s"$basename-$version.$extension"
    }
    // TODO - fix the includeUpstreamModules hack
    FileUtils.writeToFile(project.publishLocalPomFile(version, scalaVersion), PomUtils.pomXmlText(project, version, scalaVersion))
    var result = true
    if (signArtifacts)
      result = signFile(project.publishLocalPomFile(version, scalaVersion))

    result &&= Vector(
      (project.packageJar(Some(version), scalaVersion), s"${project.artifactId(scalaVersion)}-$version.jar"),
      (project.sourcePackageJar(Some(version), scalaVersion), s"${project.artifactId(scalaVersion)}-$version-sources.jar"),
      (project.docPackageJar, s"${project.artifactId(scalaVersion)}-$version-javadoc.jar")
    ).filter(_._1.exists).forall{
      case (jar, versionedBasename) => 
        val fileWithVersion = file(project.publishLocalJarDir(version, scalaVersion), versionedBasename)
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
