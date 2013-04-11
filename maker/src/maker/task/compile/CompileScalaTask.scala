package maker.task.compile

import maker.project.Project
import com.typesafe.zinc.Inputs
import scala.collection.JavaConversions._
import java.io.File
import xsbti.compile.CompileOrder
import sbt.compiler.CompileFailed
import sbt.inc.Analysis
import sbt.inc.Locate
import maker.task.Build

case class CompileScalaTask(projectPhase : ProjectPhase){

  val log = projectPhase.log

  val inputs = {
    val upstreamProjectPhases = projectPhase.strictlyUpstreamProjectPhases
    var upstreamCaches = Map[File, File]()
    upstreamProjectPhases.foreach{
      case pp : ProjectPhase ⇒ 
        upstreamCaches += (pp.outputDir → pp.compilationCacheFile)
    }
    
    val sourceFiles : Seq[File] = projectPhase.sourceFiles.toList
    val cp : Seq[File] = projectPhase.classpathDirectoriesAndJars.toList
    val outputDir = projectPhase.outputDir
    val cacheFile = projectPhase.compilationCacheFile
    val scalacOptions : Seq[String] = Nil
    val javacOptions : Seq[String] = Nil
    val analyses : Map[File, Analysis] = Map[File, Analysis]() ++ projectPhase.project.analyses
    val definesClass: File ⇒ String ⇒ Boolean = Locate.definesClass _
        
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

  }

  def exec : Either[CompileFailed, Analysis] = {
    val upstreamProjectPhases = projectPhase.strictlyUpstreamProjectPhases
    var upstreamCaches = Map[File, File]()
    upstreamProjectPhases.foreach{
      case pp : ProjectPhase ⇒ 
        upstreamCaches += (pp.outputDir → pp.compilationCacheFile)
    }
    
    val sourceFiles : Seq[File] = projectPhase.sourceFiles.toList
    val cp : Seq[File] = projectPhase.classpathDirectoriesAndJars.toList
    val outputDir = projectPhase.outputDir
    val cacheFile = projectPhase.compilationCacheFile
    val scalacOptions : Seq[String] = Nil
    val javacOptions : Seq[String] = Nil
    val analyses : Map[File, Analysis] = Map[File, Analysis]() ++ projectPhase.project.analyses
    val definesClass: File ⇒ String ⇒ Boolean = Locate.definesClass _
        
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
      val t0 = System.currentTimeMillis
      val analysis = Project.compiler.compile(inputs)(projectPhase.compilerLogger)
      Build.addCompileTime(t0, System.currentTimeMillis)
      projectPhase.project.analyses.put(outputDir, analysis)
      Right(analysis)
    } catch {
      case e : CompileFailed ⇒ 
        Left(e)
    }
    result
  }
}
