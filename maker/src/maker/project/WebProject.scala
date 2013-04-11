package maker.project

import maker.task.Build
import maker.task.tasks.RunJettyTask
import maker.utils.FileUtils._
import maker.task.tasks.PackageWarTask

trait WebProject{
  self : Project ⇒ 
    def webAppDetails : WebAppDetails
    /// run a web-application in an embedded Jetty container
    def RunJetty() = Build(RunJettyTask(this))
    /// run web-app project in an embedded Jetty container
    def runJetty = RunJetty().execute
    override def outputArtifact = file(layout.packageDir.getAbsolutePath, name + ".war")
    def PackageWar() = Build(PackageWarTask(self))
    override def pack = PackageWar().execute
}
