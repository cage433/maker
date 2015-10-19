package maker.task.compile

import maker.utils.FileUtils._
import maker.project.Module
import maker.ScalaVersion
import maker.utils.FileUtils

/**
  * Use to speed up tests that require a compilation task to execute, but have no need
  * for real class files to be produced. 
  */
case class DummyCompileTask(module : Module, phase : CompilePhase) {
  def exec() {
    module.sourceFiles(phase).foreach{
      sf =>
        val root = module.sourceDirs(phase).find(sf.isContainedIn).get
        val relativeSrcFile = sf.relativeTo(root)
        val classFile = file(module.classDirectory(phase).getAbsolutePath + "/" + relativeSrcFile.getParentFile.getPath + "/" + relativeSrcFile.getName.replace(".scala", ".class"))
        classFile.touch
    }
    val classFiles = FileUtils.findClasses(module.classDirectory(phase))
    assert(module.sourceFiles(phase).size == classFiles.size, "Should have one dummy class file for each source file")
  }
}
