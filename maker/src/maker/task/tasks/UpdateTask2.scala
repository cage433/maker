package maker.task.tasks

import maker.project.Module
import org.apache.maven.repository.internal.MavenRepositorySystemUtils
import org.eclipse.aether.graph.Dependency
import org.eclipse.aether.util.artifact.JavaScopes
import org.eclipse.aether.artifact.DefaultArtifact
import org.eclipse.aether.repository.{RemoteRepository, LocalRepository}
import org.eclipse.aether.collection.CollectRequest
import org.eclipse.aether.util.filter.DependencyFilterUtils
import org.eclipse.aether.resolution.DependencyRequest
import org.eclipse.aether.connector.basic.BasicRepositoryConnectorFactory
import org.eclipse.aether.impl.DefaultServiceLocator
import org.eclipse.aether.transport.file.FileTransporterFactory
import org.eclipse.aether.transport.http.HttpTransporterFactory
import org.eclipse.aether.RepositorySystem
import org.eclipse.aether.spi.connector.RepositoryConnectorFactory
import org.eclipse.aether.spi.connector.transport.TransporterFactory
import maker.task.{Task, TaskResult, DefaultTaskResult}
import maker.utils.Stopwatch
import scala.collection.JavaConversions._
import maker.ConfigPimps

class UpdateTask2(module : Module) 
  extends Task
  with ConfigPimps
{

  import module.config
  def name = s"Update Task 2 ${module.name}"
  def newRepositorySystem() : RepositorySystem = {
    /*
      * Aether's components implement org.eclipse.aether.spi.locator.Service to ease manual wiring and using the
      * prepopulated DefaultServiceLocator, we only need to register the repository connector and transporter
      * factories.
      */
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

  def upstreamTasks = Nil
  def exec(results : Iterable[TaskResult], sw : Stopwatch) : TaskResult = {
    val system = newRepositorySystem()
    val session = MavenRepositorySystemUtils.newSession
    val localRepo = new LocalRepository(config.resourceCache.getAbsolutePath)
    session.setLocalRepositoryManager( system.newLocalRepositoryManager( session, localRepo ) )
    val artifacts = new java.util.LinkedList[Dependency]()
    module.resources.foreach{
      resource => 
        artifacts.add(
          new Dependency(
            new DefaultArtifact(
              resource.groupId, resource.artifactId, "jar", resource.version
            ),
            JavaScopes.COMPILE
          )
        )
    }
    val central = new RemoteRepository.Builder( "central", "default", "http://repo1.maven.org/maven2/" ).build()
    val repositories = new java.util.LinkedList[RemoteRepository]()
    repositories.add(central)
    val collectRequest = new CollectRequest(artifacts, new java.util.LinkedList[Dependency](), repositories)

    val dependencyRequest = new DependencyRequest(
      collectRequest, 
      DependencyFilterUtils.classpathFilter(JavaScopes.COMPILE)
    )
    val deps = system.resolveDependencies(session, dependencyRequest)
    val artifactResults = deps.getArtifactResults()
    artifactResults.foreach(println)
    DefaultTaskResult(this, true, sw)
  }
}
