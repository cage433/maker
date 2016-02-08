package maker.task.tasks

import org.eclipse.aether.util.graph.selector._
import java.util.Arrays
import org.eclipse.aether.connector.basic.BasicRepositoryConnectorFactory
import org.eclipse.aether.impl.DefaultServiceLocator
import org.eclipse.aether.transport.file.FileTransporterFactory
import org.eclipse.aether.transport.http.HttpTransporterFactory
import org.eclipse.aether.util.artifact.JavaScopes
import org.eclipse.aether.repository.LocalRepository
import org.apache.maven.repository.internal.MavenRepositorySystemUtils
import org.eclipse.aether.spi.connector.RepositoryConnectorFactory
import org.eclipse.aether.RepositorySystem
import org.eclipse.aether.spi.connector.transport.TransporterFactory
import java.io.File
import org.eclipse.aether.installation.InstallRequest
import org.eclipse.aether.artifact.Artifact
import org.eclipse.aether.resolution.DependencyRequest
import org.eclipse.aether.collection.CollectRequest
import maker.utils.FileUtils

class AetherSystem(downloadCacheDirectory: File) extends FileUtils {

  lazy val system = {
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


  lazy val (session, localRepositoryManager) = {
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
    val localRepositoryManager = system.newLocalRepositoryManager( 
      session, 
      new LocalRepository(downloadCacheDirectory)
    )
    session.setLocalRepositoryManager(localRepositoryManager)
    (session, localRepositoryManager)
  }

  def install(installRequest: InstallRequest) {
    system.install(session, installRequest)
  }

  def absolutePathForLocalArtifact(artifact: Artifact) = file(downloadCacheDirectory, localRepositoryManager.getPathForLocalArtifact(artifact))

  def resolveDependencies(dependencyRequest: DependencyRequest) = system.resolveDependencies(session, dependencyRequest)

  def collectDependencies(collectRequest: CollectRequest) = system.collectDependencies(session, collectRequest)
}
