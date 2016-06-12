package maker.project

import maker.task.BuildResult
import maker.task.compile.{CompilePhase, SourceCompilePhase}
import maker.utils.FileUtils
import java.io.File

object Continuously {
  def apply(project: ProjectTrait, build: () => BuildResult) {
    var lastTaskTime :Option[Long] = None

    def allSourceFiles : Seq[File] = project.upstreamModules.flatMap(_.sourceFiles(SourceCompilePhase)) ++: 
    (project.testUpstreamModules.flatMap{module => 
      CompilePhase.TEST_PHASES.flatMap(module.sourceFiles(_))
    })

    def sourceFileCount : Int = allSourceFiles.size
    var lastFileCount : Int = sourceFileCount 
    def sourceFileNames : String = allSourceFiles.map(_.getPath).sortWith(_<_).mkString(" ")
    var lastSourceFileNames : String = sourceFileNames

    def printWaitingMessage = println("\nWaiting for source file changes (press 'enter' to interrupt)")
    def rerunTask{
      println((build()))
      lastTaskTime = Some(System.currentTimeMillis)
      lastFileCount = sourceFileCount
      lastSourceFileNames = sourceFileNames
      printWaitingMessage
    }


    def lastSrcModificationTime : Option[Long] = {
      FileUtils.lastModifiedFileTime(allSourceFiles)
    }
    printWaitingMessage
    while (true) {
      Thread.sleep(1000)
      if (System.in.available > 0 && System.in.read == 10) return
      (lastTaskTime,  lastSrcModificationTime, lastFileCount, sourceFileCount, lastSourceFileNames, sourceFileNames) match {
        case (None, _, _, _, _, _) => { rerunTask }                        // Task has never been run
        case (Some(t1), Some(t2), _, _, _, _) if t1 < t2 => { rerunTask }  // Code has changed since task last run
        case (_, _, m, n, _, _) if m != n => { rerunTask }                  // Source file has been added or deleted
        case (_, _, _, _, a, b) if a != b => { rerunTask }                  // Source file has been renamed
        case _ =>                                                    // Either no code yet or code has not changed
      }
    }
  }
}
