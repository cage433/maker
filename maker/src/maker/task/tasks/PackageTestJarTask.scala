package maker.task.tasks

import java.io.File
import maker.project.Module
import maker.task.compile.TestCompileTask
import maker.utils.FileUtils._

case class PackageTestJarTask(module: Module) extends PackageJarTask(module) {
  def name = "Package Tests Jar"
  def upstreamTasks = TestCompileTask(module) :: module.immediateUpstreamModules.map(PackageTestJarTask)

  protected def outputArtifact: File = module.testOutputArtifact
  protected def outputDir: File = module.testCompilePhase.outputDir
  protected def resourceDir: File = module.testResourceDir
}
