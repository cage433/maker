package maker.task.tasks

import maker.project._
import java.util.Date
import org.apache.ivy.core.publish.PublishOptions
import org.apache.ivy.util.filter.FilterHelper
import org.apache.ivy.core.resolve.ResolveOptions
import org.apache.ivy.Ivy
import maker.task._
import maker.utils.Stopwatch
import maker.MakerProps
import maker.utils.maven.IvyLock
import scala.collection.JavaConversions._


case class PublishTask(baseProject : BaseProject, resolverName : String, version : String) extends Task {

  def name = "Publish"
  def module = baseProject
  def upstreamTasks = PublishLocalTask(baseProject, version = version) :: baseProject.immediateUpstreamModules.map(PublishTask(_, resolverName, version))
  def exec(results : Iterable[TaskResult], sw : Stopwatch) = {
    IvyLock.synchronized{
      doPublish(baseProject, results, sw)
    }
  }

  private def doPublish(baseProject: BaseProject, results : Iterable[TaskResult], sw : Stopwatch) = {

    val props : MakerProps = baseProject.props

    val ivyFile = baseProject.ivyFile
    try {
      val confs = Array[String]("default")


      val ivy = {
        val ivy = Ivy.newInstance
        ivy.getSettings.addAllVariables(System.getProperties)
        ivy.configure(baseProject.ivySettingsFile)
        ivy
      }


      val moduleRevisionID = {
        val resolveOptions = new ResolveOptions().setConfs(confs)
          .setValidate(true)
          .setArtifactFilter(FilterHelper.getArtifactTypeFilter(Array[String]("xml", "jar", "bundle", "source")))
        val report = ivy.resolve(ivyFile.toURI().toURL(), resolveOptions)
        val md = report.getModuleDescriptor
        md.getModuleRevisionId()
      }

      val publishOptions = new PublishOptions()
                            .setConfs(confs).setOverwrite(true)
                            .setPubrevision(version)
                            .setPubdate(new Date())

      val srcArtifactPattern = List(
        baseProject.publishLocalDir.getAbsolutePath + "/[type]s/pom.xml",
        baseProject.publishLocalDir.getAbsolutePath + "/[type]s/" + baseProject.artifactId + ".jar")


      ivy.publish(
        moduleRevisionID,
        srcArtifactPattern,
        resolverName,
        publishOptions)

      DefaultTaskResult(this, true, sw)
    }
    catch {
      case e: Throwable =>
        e.printStackTrace
        DefaultTaskResult(this, false, sw, exception = Some(e))
    }
  }
}
