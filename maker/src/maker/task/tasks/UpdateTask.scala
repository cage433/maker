package maker.task.tasks

import maker.project._
import maker.utils.FileUtils._
import maker.task._
import maker.utils._
import maker.{ConfigPimps, ScalaVersion}
import scala.collection.JavaConversions._
import java.net.URL
import java.io.File
import org.eclipse.aether.connector.basic.BasicRepositoryConnectorFactory
import org.eclipse.aether.collection.{CollectRequest, CollectResult}
import org.eclipse.aether.impl.DefaultServiceLocator
import org.eclipse.aether.util.filter.{DependencyFilterUtils, AndDependencyFilter, ExclusionsDependencyFilter}
import org.eclipse.aether.resolution.DependencyRequest
import org.eclipse.aether.transport.file.FileTransporterFactory
import org.eclipse.aether.transport.http.HttpTransporterFactory
import org.eclipse.aether.util.artifact.JavaScopes
import org.eclipse.aether.repository.{LocalRepository, RemoteRepository}
import org.apache.maven.repository.internal.MavenRepositorySystemUtils
import org.eclipse.aether.spi.connector.RepositoryConnectorFactory
import org.eclipse.aether.RepositorySystem
import org.eclipse.aether.spi.connector.transport.TransporterFactory
import org.eclipse.aether.artifact.{Artifact, DefaultArtifact}
import org.apache.commons.io.{FileUtils => ApacheFileUtils}
import org.eclipse.aether.graph.{Dependency, DependencyNode}
import com.typesafe.config.Config
import org.eclipse.aether.util.graph.selector._
import java.util.Arrays
import org.eclipse.aether.internal.test.util.DependencyGraphParser


/**
  * Updates any missing dependencies. If any jars are missing then will try 
  * to download BOTH the binary and any associated source jar. 
  * If `forceSourceUpdate` is true then will try to download ALL missing source jars 
  *
  * Missing source jars are not treated as a cause for failure unless `forceSourceUpdate`
  * is true
  */
case class UpdateTask(project : ProjectTrait, scalaVersion : ScalaVersion) 
  extends Task
  with ConfigPimps
  with EitherPimps
  with DependencyPimps
  with StringBufferPimps
{
  def name = "Update " + project

  def upstreamTasks : List[Task] = Nil
  import project.config

  private val (system, session, repositories) = UpdateTask.aetherState(config)

  def exec(results : Iterable[TaskResult], sw : Stopwatch) : TaskResult = {
    val result = if (project.dependenciesAlreadyUpdated(scalaVersion)){
      logger.info(s"$project has up to date dependencies")
      Right(Unit)
    } else {
      updateDependencies(BinaryDownload(JavaScopes.COMPILE)) andThen 
      updateDependencies(BinaryDownload(JavaScopes.TEST)) andThen {
        if (project.updateIncludesSourceJars){
          updateDependencies(SourceDownload(JavaScopes.COMPILE)) andThen
          updateDependencies(SourceDownload(JavaScopes.TEST)) 
        } else 
          Right(Unit)
      }
    }
    result match {
      case Right(_) => 
        project.markDependenciesUpdated(scalaVersion)
        DefaultTaskResult(this, true, sw)
      case Left(ex) => 
        DefaultTaskResult(
          this,
          succeeded = false,
          stopwatch = sw,
          message = Some(ex.getMessage),
          exception = Some(ex)
        )
    }
  }

  trait DownloadType{
    def scope : String
    def downloadDirectory : File
    def isOfCorrectType(artifact : Artifact) : Boolean
    def aetherDependencies(dpes : Seq[RichDependency]) : Seq[Dependency]
  }

  case class BinaryDownload(scope : String) extends DownloadType{
    def isOfCorrectType(artifact : Artifact) = true
    def downloadDirectory = scope match {
      case JavaScopes.COMPILE => project.managedLibDir(scalaVersion)
      case JavaScopes.TEST => project.testManagedLibDir(scalaVersion)
      case _ => ???
    }
    def aetherDependencies(deps : Seq[RichDependency]) : Seq[Dependency] = {
      deps.map(_.aetherDependency(scalaVersion))
    }
  }

  case class SourceDownload(scope : String) extends DownloadType{
    // Aether downloads the binary when sources aren't in the repository - no idea why
    def isOfCorrectType(artifact : Artifact) = Option(artifact.getClassifier) == Some("sources")
    def downloadDirectory = scope match {
      case JavaScopes.COMPILE => project.managedLibSourceDir(scalaVersion)
      case JavaScopes.TEST => project.testManagedLibSourceDir(scalaVersion)
      case _ => ???
    }
    def aetherDependencies(deps : Seq[RichDependency]) : Seq[Dependency] = {
      deps.map(_.withClassifier("sources").aetherDependency(scalaVersion))
    }
  }

  private def getArtifacts(download : DownloadType) : Seq[Artifact] = {
    logger.info(s"Getting artifacts for $this - $download")
    var aetherDependencies = download.aetherDependencies(
      scalaVersion.scalaLibraryRichDependency +: 
      scalaVersion.scalaCompilerRichDependency +:
      project.upstreamDependencies
    )

    val collectRequest = new CollectRequest(aetherDependencies, new java.util.LinkedList[Dependency](), repositories)
    val dependencyRequest = new DependencyRequest(
      collectRequest,
      DependencyFilterUtils.classpathFilter(download.scope)
    )

    val artifacts = system.resolveDependencies(
      session, 
      dependencyRequest
    ).getArtifactResults.map(_.getArtifact).filter(download.isOfCorrectType)

    collectRequest.setRepositories(repositories)
    val collectResult : CollectResult = system.collectDependencies(session, collectRequest)

    FileUtils.writeToFile(
      file(download.downloadDirectory, "dependency-graph"), 
      new DependencyGraphParser().dump(collectResult.getRoot)
    )

    artifacts
  }

  // Used for bootstrapping
  def binaryArtifacts = getArtifacts(BinaryDownload(JavaScopes.COMPILE))

  private def updateDependencies(download : DownloadType) : Either[Exception, Unit] = {
    try {
      val dependencyFiles = getArtifacts(download).map(_.getFile)
      logger.info(s"Purging ${download.downloadDirectory}")
      cleanRegularFilesLeavingDirectories(download.downloadDirectory)
      dependencyFiles.foreach{
        file => 
          logger.info(s"Adding dependency ${file.basename}")
          ApacheFileUtils.copyFileToDirectory(file, download.downloadDirectory)
      }
      
      Right(Unit)
    } catch {
      case e : Exception => 
        Left(e)
    }
  }

}

object UpdateTask extends ConfigPimps {
  def aetherState(config : Config) = {
    val system = {
      val locator = MavenRepositorySystemUtils.newServiceLocator()
      locator.addService( classOf[RepositoryConnectorFactory], classOf[BasicRepositoryConnectorFactory] )
      locator.addService( classOf[TransporterFactory], classOf[FileTransporterFactory] )
      locator.addService( classOf[TransporterFactory],  classOf[HttpTransporterFactory] )

      locator.setErrorHandler( new DefaultServiceLocator.ErrorHandler()
      {
          override def serviceCreationFailed( type_ : Class[_] , impl : Class[_], exception : Throwable )
          {
              exception.printStackTrace()
          }
      } )

      locator.getService( classOf[RepositorySystem] )
    }

    val session = {
      val session = MavenRepositorySystemUtils.newSession

      session.setDependencySelector(
        new AndDependencySelector(
          new OptionalDependencySelector(),
          new ExclusionDependencySelector(),
          new ScopeDependencySelector(
            Arrays.asList(JavaScopes.COMPILE),
            Arrays.asList(JavaScopes.TEST, JavaScopes.SYSTEM, JavaScopes.PROVIDED)
          )
        )
      )
      session.setLocalRepositoryManager( 
        system.newLocalRepositoryManager( 
          session, 
          new LocalRepository(config.resourceCache.getAbsolutePath)
        )
      )
      session
    }
    (system, session, repositories(config))
  }

  def repositories(config : Config) = {
    val repos = new java.util.LinkedList[RemoteRepository]()
    config.httpResolvers.foreach{
      case List(name, url) => 
        repos.add(new RemoteRepository.Builder(name, "default", url).build())
    }
    repos
  }

  def reportOnUpdateFailures(taskResults : List[TaskResult]){
    import maker.utils.RichString._
    val failures : List[(Int, String)] = taskResults.collect{
      case u : UpdateTaskResult => u.failures
    }.flatten
    if (failures.nonEmpty){
      val b = new StringBuffer
      val tb = TableBuilder("Curl Error Code   ", "URL")
      failures.foreach{
        case (returnCode, command) => 
          tb.addRow(returnCode.toString, command)
      }
      b.addLine("\n" + tb.toString)
      b.addLine("\n\n" + "Proxy settings may be the cause - env vars are ".inRed)
      val etb = TableBuilder("Variable              ", "Value")
      System.getenv().filterKeys(_.toLowerCase.contains("proxy")).foreach{
        case (variable, value) => 
          etb.addRow(variable, value.truncate(100))
      }
      b.addLine(etb.toString)
      println(b)
    }
  }
}

case class UpdateTaskResult(
  task : UpdateTask, 
  succeeded : Boolean, 
  stopwatch : Stopwatch,
  failures : Seq[(Int, String)],
  override val message : Option[String] = None, 
  override val exception : Option[Throwable] = None
) extends TaskResult
