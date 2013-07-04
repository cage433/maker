package maker.task.compile

import maker.task.TaskResult
import maker.utils.FileUtils._

/**
  * Use to speed up tests that require a compilation task to execute, but have no need
  * for real class files to be produced. 
  */
case class DummyCompileTask(mp : ModuleCompilePhase) {
  def exec {
    mp.sourceFiles.foreach{
      sf =>   
        val relativeSrcFile = sf.relativeTo(mp.sourceDir)
        val classFile = file(mp.outputDir.getAbsolutePath + "/" + relativeSrcFile.getParentFile.getPath + "/" + relativeSrcFile.getName.replace(".scala", ".class"))
        classFile.touch
    }
    assert(mp.sourceFiles.size == mp.classFiles.size, "Should have one dummy class file for each source file")
  }
}
