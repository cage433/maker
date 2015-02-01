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
import org.apache.ivy.util.{DefaultMessageLogger, Message}


case class PublishTask(
  baseProject : BaseProject, 
  modules : Seq[Module],
  resolverName : String, 
  version : String, 
  signArtifacts : Boolean
) 
  extends Task 
{

  def name = "Publish"
  def module = baseProject
  def baseProjects = Vector(baseProject)
  def upstreamTasks = PublishLocalTask(baseProject, modules, version, signArtifacts) :: Nil
  def exec(results : Iterable[TaskResult], sw : Stopwatch) = {
    IvyLock.synchronized{
      doPublish(baseProject, results, sw)
    }
  }

  private def doPublish(baseProject: BaseProject, results : Iterable[TaskResult], sw : Stopwatch) = {

    val props : MakerProps = baseProject.props

    val ivyFile = baseProject.ivyFile(version)
    try {
      val confs = Array[String]("default")


      val ivy = {
        val ivy = Ivy.newInstance
        if (baseProject.isTestProject)
          ivy.getLoggerEngine.setDefaultLogger(new DefaultMessageLogger(Message.MSG_ERR))
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
        baseProject.publishLocalDir(version).getAbsolutePath + "/[type]s/pom.xml",
        baseProject.publishLocalDir(version).getAbsolutePath + "/[type]s/" + baseProject.artifactId  + "-[revision]" + "(-[classifier]).jar"
      )


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
