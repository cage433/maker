package maker.task.tasks

import maker.project.Module
import maker.utils.FileUtils._
import maker.task._
import maker.utils.{Stopwatch, TableBuilder, Int}
import maker.{Resource, ResourceUpdater, ConfigPimps}
import maker.utils.RichString._
import scala.collection.JavaConversions._
import org.scalatest.Failed
import java.net.URL
import java.io.File
import org.eclipse.aether.connector.basic.BasicRepositoryConnectorFactory
import org.eclipse.aether.collection.CollectRequest
import org.eclipse.aether.impl.DefaultServiceLocator
import org.eclipse.aether.util.filter.DependencyFilterUtils
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
import org.eclipse.aether.graph.Dependency
import com.typesafe.config.Config

/**
  * Updates any missing resources. If any jars are missing then will try 
  * to download BOTH the binary and any associated source jar. 
  * If `forceSourceUpdate` is true then will try to download ALL missing source jars 
  *
  * Missing source jars are not treated as a cause for failure unless `forceSourceUpdate`
  * is true
  */
case class UpdateTask(module : Module, forceSourceUpdate : Boolean) 
  extends Task
  with ConfigPimps
{
  def baseProject = module
  def name = "Update " + module

  def upstreamTasks : List[Task] = Nil
  import module.config

  private val (system, session, repositories) = UpdateTask.aetherState(config)

  def exec(results : Iterable[TaskResult], sw : Stopwatch) : TaskResult = {
    val artifacts = module.resources.map{
      resource => 
        new DefaultArtifact(
          resource.groupId, resource.artifactId, "jar", resource.version
        )
    }
    val result = updateDependencies(artifacts, BinaryDownload) 
    result match {
      case Right(hasChanged) => 
        if (hasChanged || forceSourceUpdate){
          val sourceArtifacts = module.resources.map{
            resource => 
              new DefaultArtifact(
                resource.groupId, resource.artifactId, "sources", "jar", resource.version
              )
          }
          updateDependencies(sourceArtifacts, SourceDownload)
        }
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

  abstract class DownloadType(val directory : File){
    def isOfCorrectType(file : File) : Boolean
  }

  object BinaryDownload extends DownloadType(module.managedLibDir){
    def isOfCorrectType(file : File) = true
  }

  object SourceDownload extends DownloadType(module.managedLibSourceDir){
    // Aether downloads the binary when sources aren't in the repository - no idea why
    def isOfCorrectType(file : File) = file.basename.contains("sources")
  }

  private def updateDependencies(artifacts : Seq[Artifact], download : DownloadType) : Either[Exception, Boolean] = {
    def addMissingDependencies(dependencyFiles : Seq[File]) : Boolean = {
      val currentBasenames = download.directory.safeListFiles.map(_.basename).toSet
      var hasChanged = false
      dependencyFiles.foreach{
        file => 
          if (! currentBasenames.contains(file.basename)){
            logger.info(s"Adding dependency ${file.basename}")
            ApacheFileUtils.copyFileToDirectory(file, download.directory)
            hasChanged = true
          }
      }
      hasChanged
    }
    def removeRedundantDependencies(dependencyFiles : Seq[File]) : Boolean = {
      val dependencyBasenames = dependencyFiles.map(_.basename).toSet
      var hasChanged = false
      download.directory.safeListFiles.foreach{
        file => 
          if (! dependencyBasenames.contains(file.basename)){
            logger.info("Removing redundant dependency " + file)
            file.delete
            hasChanged = true
          }
      }
      hasChanged
    }
    try {
      val dependencies = artifacts.map(new Dependency(_, JavaScopes.COMPILE))
      val dependencyRequest = new DependencyRequest(
        new CollectRequest(dependencies, new java.util.LinkedList[Dependency](), repositories),
        DependencyFilterUtils.classpathFilter(JavaScopes.COMPILE)
      )
      val dependencyFiles = 
        system.resolveDependencies(
          session, 
          dependencyRequest
        ).getArtifactResults.map(_.getArtifact.getFile).filter(download.isOfCorrectType)
      var hasChanged : Boolean = removeRedundantDependencies(dependencyFiles)
      hasChanged = addMissingDependencies(dependencyFiles) || hasChanged
      Right(hasChanged)
    } catch {
      case e : Exception => 
        Left(e)
    }
  }

}

object UpdateTask extends ConfigPimps{
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

      session.setLocalRepositoryManager( 
        system.newLocalRepositoryManager( 
          session, 
          new LocalRepository(config.resourceCache.getAbsolutePath)
        )
      )
      session
    }
    val repositories = {
      val repos = new java.util.LinkedList[RemoteRepository]()
      config.httpResolvers.foreach{
        case List(name, url) => 
          repos.add(new RemoteRepository.Builder(name, "default", url).build())
      }
      repos
    }
    (system, session, repositories)
  }
  def reportOnUpdateFailures(taskResults : List[TaskResult]){
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
