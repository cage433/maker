package maker.project

import maker.task.compile.CompilePhase
import maker.utils.FileUtils

trait ModuleTestPimps{
  implicit class PimpedModule(module : Module){
    def classFiles(phase : CompilePhase) = FileUtils.findClasses(module.classDirectory(phase))
  }
}
