package maker.task.tasks

import org.apache.commons.io.FileUtils._
import maker.project._
import maker.task._
import maker.utils._
import maker._
import maker.task.compile.SourceCompilePhase
import java.io.File
import maker.utils.os.Command
import org.scalatest.Failed
import org.eclipse.aether.artifact.{Artifact, DefaultArtifact}
import org.eclipse.aether.util.artifact.SubArtifact
import org.eclipse.aether.installation.InstallRequest
import scala.collection.mutable.Publisher

case class PublishLocalTask(
  project : Project, 
  version : String,
  signArtifacts : Boolean
) 
  extends Task 
  with Log
  with FileUtils
  with EitherPimps
{
  def name = "Publish Local"

  val system = new AetherSystem(project.resourceCacheDirectory)

  def upstreamTasks : List[Task] = List(DocTask(project))

  type ErrorMessage = String

  def exec(results : Iterable[TaskResult], sw : Stopwatch) = {
    withTempDir {
      tempDir => 

        Publisher(tempDir).install() match {
          case Left(errorMessage) => 
            DefaultTaskResult(this, succeeded = false, stopwatch = sw, message = Some(errorMessage))
          case Right(_) => 
            DefaultTaskResult(this, true, sw)
        }
    }
  }
  
  case class Publisher(tempDir: File) extends Log {
    val tempClassJar = file(tempDir, "classes.jar")
    val tempSourceJar = file(tempDir, "sources.jar")
    val tempDocJar = file(tempDir, "doc.jar")
    val tempPomFile = file(tempDir, "pom.pom")

    lazy val installRequest: InstallRequest = new InstallRequest().
      addArtifact(project.jarArtifact(version).setFile(tempClassJar)).
      addArtifact(project.srcArtifact(version).setFile(tempSourceJar)).
      addArtifact(project.docArtifact(version).setFile(tempDocJar)).
      addArtifact(project.pomArtifact(version).setFile(tempPomFile))

    def buildJars(): Either[ErrorMessage, Unit] = {
      val modules = project.upstreamModules
      import BuildJar.{build => buildJar}
      buildJar(
        tempClassJar,
        modules.map(_.classDirectory(SourceCompilePhase)) ++ modules.map(_.resourceDir(SourceCompilePhase))
      ) andThen
      buildJar(
        tempSourceJar,
        modules.flatMap(_.sourceDirs(SourceCompilePhase))
      ) andThen
      buildJar(
        tempDocJar,
        project.docOutputDir :: Nil
      )
    }

    private def signFile(file: File): Either[ErrorMessage, Unit] = {
      logger.debug(s"Signing $file")
      if (! file.exists) {
        val msg = s"File $file doesn't exist"
        logger.error(msg)
        Left(msg)
      } else {
        val signatureFile = new File(file.getAbsolutePath + ".asc")
        if (signatureFile.exists)
          signatureFile.delete
        val cmd = Command("gpg", "-ab", "--passphrase", project.gpgPassPhrase, file.getAbsolutePath)
        val result = cmd.run
        if (result != 0)
          Left("Failed to sign " + file)
        else
          Right(Unit)
      }
    }

    def install() : Either[ErrorMessage, Unit] = {
      FileUtils.writeToFile(tempPomFile, PomUtils.pomXmlText(project, version))
      buildJars() andThen {
        system.install(installRequest)
        Right(Unit)
      } andThen {
        if (signArtifacts) {
          signFile(project.publishedLocalJar(version))        andThen 
          signFile(project.publishedLocalSourcesJar(version)) andThen 
          signFile(project.publishedLocalJavadocJar(version)) andThen 
          signFile(project.publishedLocalPom(version))
        } else {
          Right(Unit)
        }
      }
    }
  }
}


