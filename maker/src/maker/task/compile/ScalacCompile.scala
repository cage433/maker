package maker.task.compile

import maker.project.Module
import com.typesafe.zinc.Inputs
import com.typesafe.zinc.IncOptions
import scala.collection.JavaConversions._
import java.io.File
import xsbti.compile.CompileOrder
import sbt.compiler.CompileFailed
import sbt.inc.Analysis
import sbt.inc.Locate
import maker.build.Build
import maker.utils.FileUtils._

object ScalacCompile{

  def apply(modulePhase : ModuleCompilePhase) : Either[CompileFailed, Analysis] = {
    val outputDir = modulePhase.outputDir.getCanonicalFile
        
    def inputs = Inputs(
      classpath         = modulePhase.classpathDirectoriesAndJars.toList.map(_.getCanonicalFile),
      sources           = modulePhase.sourceFiles.toList.map(_.getCanonicalFile),
      classesDirectory  = outputDir,
      scalacOptions     = Nil,
      javacOptions      = Nil,
      cacheFile         = modulePhase.compilationCacheFile,
      analysisMap       = Map[File, Analysis]() ++ modulePhase.module.analyses,
      forceClean        = false,
      definesClass      = Locate.definesClass _,
      javaOnly          = false,
      compileOrder      = CompileOrder.Mixed,
      incOptions        = IncOptions(),
      outputRelations   = None,
      outputProducts    = None,
      mirrorAnalysis    = true
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
