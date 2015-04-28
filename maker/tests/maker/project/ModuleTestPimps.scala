package maker.project

import maker.task.compile.CompilePhase

trait ModuleTestPimps{
  implicit class PimpedModule(module : Module){
    def classFiles(phase : CompilePhase) = module.classFiles(module.defaultScalaVersion, phase)
    def classDirectory(phase : CompilePhase) = module.classDirectory(module.defaultScalaVersion, phase)
  }
}
