package maker.task.tasks

import org.apache.commons.io.FileUtils._
import maker.project._
import maker.task._
import maker.utils.{Stopwatch, FileUtils}
import maker._
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
  with Log
{
  def name = "Publish Local"

  def upstreamTasks : List[Task] = List(PackageJarTask(project, Some(version), scalaVersion))

  def exec(results : Iterable[TaskResult], sw : Stopwatch) = {
    doPublish(project, results, sw)
  }
  
  private def signFile(file : File) = {
    val signatureFile = new File(file.getAbsolutePath + ".asc")
    if (signatureFile.exists)
      signatureFile.delete
    val cmd = Command("gpg", "-ab", "--passphrase", project.gpgPassPhrase, file.getAbsolutePath)
    val result = cmd.run
    if (result != 0)
      logger.error("Failed to sign " + file)
    result == 0
  }

  private def doPublish(project: Project, results : Iterable[TaskResult], sw : Stopwatch) = {
  
    project.publishLocalDir(version).deleteAll()
    def versionedFilename(file : File) : String = {
     val basename :: extension :: Nil = file.basename.split('.').toList
     s"$basename-$version.$extension"
    }
    // TODO - fix the includeUpstreamModules hack
    FileUtils.writeToFile(project.publishLocalPomFile(version), PomUtils.pomXmlText(project, version))
    var result = true
    if (signArtifacts)
      result = signFile(project.publishLocalPomFile(version))

    result &&= Vector(
      (project.packageJar(Some(version)), s"${project.artifactId}-$version.jar"),
      (project.sourcePackageJar(Some(version)), s"${project.artifactId}-$version-sources.jar"),
      (project.docPackageJar, s"${project.artifactId}-$version-javadoc.jar")
    ).filter(_._1.exists).forall{
      case (jar, versionedBasename) => 
        val fileWithVersion = file(project.publishLocalJarDir(version), versionedBasename)
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
