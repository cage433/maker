package maker.task.compile

import maker.project.Module
import com.typesafe.zinc.Inputs
import scala.collection.JavaConversions._
import java.io.File
import xsbti.compile.CompileOrder
import sbt.compiler.CompileFailed
import sbt.inc.Analysis
import sbt.inc.Locate
import maker.utils.FileUtils._
import java.io.BufferedWriter
import maker.utils.Stopwatch


case class CompileScalaTask(modulePhase : ModuleCompilePhase) {
  import CompileScalaTask._

  val log = modulePhase.log

  val sourceFiles : Seq[File] = modulePhase.sourceFiles.toList

  def exec(sw : Stopwatch) : Either[CompileFailed, Analysis] = {
    val upstreamProjectPhases = modulePhase.strictlyUpstreamProjectPhases
    var upstreamCaches = Map[File, File]()
    upstreamProjectPhases.foreach{
      case pp : ModuleCompilePhase =>
        upstreamCaches += (pp.outputDir -> pp.compilationCacheFile)
    }
    
    val sourceFiles : Seq[File] = modulePhase.sourceFiles.toList
    val cp : Seq[File] = modulePhase.classpathDirectoriesAndJars.toList
    val outputDir = modulePhase.outputDir
    val cacheFile = modulePhase.compilationCacheFile
    val scalacOptions : Seq[String] = modulePhase.module.scalacOptions 
    val javacOptions : Seq[String] = modulePhase.module.javacOptions 
    val analyses : Map[File, Analysis] = Map[File, Analysis]() ++ modulePhase.module.analyses
    val definesClass: File => String => Boolean = Locate.definesClass _
        
    val inputs = Inputs(
      cp.map(_.getCanonicalFile),
      sourceFiles.map(_.getCanonicalFile),
      outputDir.getCanonicalFile,
      scalacOptions,
      javacOptions,
      cacheFile,
      analyses,
      false,
      definesClass,
      false,
      CompileOrder.Mixed,
      // zinc 0.3.5
      com.typesafe.zinc.IncOptions(),
      None,
      None,
      mirrorAnalysis = true
    )


    sw.startInterval(CompileTask.CALL_TO_COMPILER)
    val result = try {
      val analysis = Module.compiler.compile(inputs)(modulePhase.compilerLogger)
      modulePhase.module.analyses.put(outputDir, analysis)
      sw.endInterval(CompileTask.CALL_TO_COMPILER)
      Right(analysis)
    } catch {
      case e : CompileFailed => 
        sw.endInterval(CompileTask.CALL_TO_COMPILER)
        CompileScalaTask.appendCompileOutputToTopLevel(modulePhase)
        Left(e)
    }
    result
  }
}

object CompileScalaTask{
  def appendCompileOutputToTopLevel(modulePhase : ModuleCompilePhase) = synchronized {
    withFileAppender(modulePhase.module.props.VimErrorFile()){
      writer : BufferedWriter =>
        modulePhase.vimCompileOutputFile.readLines.foreach(writer.println)
    }
  }
}
