package maker.task.compile

import maker.project.Module
import com.typesafe.zinc.Inputs
import scala.collection.JavaConversions._
import java.io.File
import xsbti.compile.CompileOrder
import sbt.compiler.CompileFailed
import sbt.inc.Analysis
import sbt.inc.Locate
import maker.task.Build
import maker.utils.FileUtils._

case class CompileScalaTask(modulePhase : ModuleCompilePhase){

  val log = modulePhase.log

  val sourceFiles : Seq[File] = modulePhase.sourceFiles.toList

  //val inputs : Unit = {
    //val upstreamProjectPhases = modulePhase.strictlyUpstreamProjectPhases
    //var upstreamCaches = Map[File, File]()
    //upstreamProjectPhases.foreach{
      //case pp : ModuleCompilePhase =>
      //upstreamCaches += (pp.outputDir -> pp.compilationCacheFile)
      //}
      // 
      //val cp : Seq[File] = modulePhase.classpathDirectoriesAndJars.toList
      //val outputDir = modulePhase.outputDir
      //val cacheFile = modulePhase.compilationCacheFile
      //val scalacOptions : Seq[String] = Nil
      //val javacOptions : Seq[String] = Nil
      //val analyses : Map[File, Analysis] = Map[File, Analysis]() ++ modulePhase.module.analyses
      //val definesClass: File => String => Boolean = Locate.definesClass _
      // 
      //val inputs = Inputs(
        //cp.map(_.getCanonicalFile),
        //sourceFiles.map(_.getCanonicalFile),
        //outputDir.getCanonicalFile,
        //scalacOptions,
        //javacOptions,
        //cacheFile,
        //analyses,
        //false,
        //definesClass,
        //false,
        //CompileOrder.Mixed,
        //None,
        //None,
        //mirrorAnalysis = true
        //)
      //
      //}

  def exec : Either[CompileFailed, Analysis] = {
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
    val scalacOptions : Seq[String] = Nil
    val javacOptions : Seq[String] = Nil
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
      None,
      None,
      mirrorAnalysis = true
    )


    val result = try {
      val analysis = Module.compiler.compile(inputs)(modulePhase.compilerLogger)
      modulePhase.module.analyses.put(outputDir, analysis)
      Right(analysis)
    } catch {
      case e : CompileFailed => 
        Left(e)
    }
    result
  }
}
