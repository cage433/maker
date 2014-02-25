package maker.task.compile

import maker.project.Module
import com.typesafe.zinc.Inputs
import scala.collection.JavaConversions._
import java.io.File
import xsbti.compile.CompileOrder
import sbt.compiler.CompileFailed
import sbt.inc.Analysis
import sbt.inc.Locate
import maker.build.Build
import maker.utils.FileUtils._
import scala.util.Properties

object ScalacCompile{

  def apply(modulePhase : ModuleCompilePhase) : Either[CompileFailed, Analysis] = {
    val outputDir = modulePhase.outputDir.getCanonicalFile
        
    val inputs = Inputs(
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
      outputRelations   = None,
      outputProducts    = None,
      mirrorAnalysis    = true
    )

    def stopUsingJavaClasspath{
      /* Without this the compiler will include maker's own classpath
         This property must be set in order to launch the repl, or execute
         the compiler form the command line. 
         Not sure why - nor whether it's truly ok to unset this here
      */
      Properties.setProp("scala.usejavacp", "false")
    }

    val result = try {
      stopUsingJavaClasspath
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
