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

case class PublishLocalTask2(
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

    lazy val jarArtifact = new DefaultArtifact(project.organization, project.artifactId, "", "jar", version).setFile(tempClassJar)
    lazy val srcArtifact = new DefaultArtifact(project.organization, project.artifactId, "sources", "jar", version).setFile(tempSourceJar)
    lazy val docArtifact = new DefaultArtifact(project.organization, project.artifactId, "javadoc", "jar", version).setFile(tempDocJar)
    lazy val pomArtifact: Artifact = new SubArtifact(jarArtifact, "", "pom" ).setFile(tempPomFile)
    lazy val installRequest: InstallRequest = new InstallRequest().addArtifact(jarArtifact).addArtifact(srcArtifact).addArtifact(docArtifact).addArtifact(pomArtifact)

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
      println(s"Signing $file")
      if (! file.exists) 
        logger.error(s"File $file doesn't exist")
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

    def install() : Either[ErrorMessage, Unit] = {
      FileUtils.writeToFile(tempPomFile, PomUtils.pomXmlText(project, version))
      buildJars() andThen {
        system.install(installRequest)
        Right(Unit)
      } andThen {
        signFile(system.absolutePathForLocalArtifact(jarArtifact))
      } andThen {
        signFile(system.absolutePathForLocalArtifact(srcArtifact))
      } andThen {
        signFile(system.absolutePathForLocalArtifact(docArtifact))
      } andThen {
        signFile(system.absolutePathForLocalArtifact(pomArtifact))
      }
    }
  }
}


