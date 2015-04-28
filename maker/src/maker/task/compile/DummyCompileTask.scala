package maker.task.compile

import maker.utils.FileUtils._
import maker.project.Module

/**
  * Use to speed up tests that require a compilation task to execute, but have no need
  * for real class files to be produced. 
  */
case class DummyCompileTask(module : Module, phase : CompilePhase, majorScalaVersion : String) {
  def exec() {
    module.sourceFiles(phase).foreach{
      sf =>
        val root = module.sourceDirs(phase).find(sf.isContainedIn).get
        val relativeSrcFile = sf.relativeTo(root)
        val classFile = file(module.classDirectory(majorScalaVersion, phase).getAbsolutePath + "/" + relativeSrcFile.getParentFile.getPath + "/" + relativeSrcFile.getName.replace(".scala", ".class"))
        classFile.touch
    }
    assert(module.sourceFiles(phase).size == module.classFiles(majorScalaVersion, phase).size, "Should have one dummy class file for each source file")
  }
}
