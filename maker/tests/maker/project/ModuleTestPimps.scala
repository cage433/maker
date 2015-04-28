package maker.project

import maker.task.compile.CompilePhase

trait ModuleTestPimps{
  implicit class PimpedModule(module : Module){
    def classFiles(phase : CompilePhase) = module.classFiles(module.defaultMajorScalaVersion, phase)
    def classDirectory(phase : CompilePhase) = module.classDirectory(module.defaultMajorScalaVersion, phase)
  }
}
