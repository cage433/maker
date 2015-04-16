package maker.task.compile

import maker.task.Dependency
import maker.utils.FileUtils._
import maker.project.Module
import sbt.ConsoleLogger
import org.apache.commons.io.output.{TeeOutputStream, NullOutputStream}
import ch.qos.logback.classic.Logger
import org.slf4j.LoggerFactory
import java.io.{File, FileOutputStream, PrintStream}

case class ModuleCompilePhase(module : Module, phase : CompilePhase){

  def sourceDirs = module.sourceDirs(phase)

  def outputDir : File = {
    val f = module.outputDir(phase)
    f.mkdirs
    f
  }


  def resourceDir : File = {
    val d = module.resourceDir(phase)
    d.mkdirs
    d
  }

  def managedResourceDir: File = {
    val d = module.managedResourceDir
    d.mkdirs
    d
  }

  def scalaFiles = findFilesWithExtension("scala", sourceDirs)
  def javaFiles = findFilesWithExtension("java", sourceDirs)

  def sourceFiles = scalaFiles ++ javaFiles
  def classFiles : Iterable[File] = {
    findClasses(outputDir)
  }
  def classNames : Iterable[String] = {
    // used for class dependency analysis
    classFiles.map(_.relativeTo(outputDir)).map(_.getPath).filterNot(_.contains("$$")).map(_.replace('/', '.').dropRight(6)) 
  }

  def strictlyUpstreamProjectPhases = {
    var projectPhases = scala.collection.mutable.Set[ModuleCompilePhase]()
    for {
      m <- module.allStrictlyUpstreamTestModules
      phase <- SourceCompilePhase :: TestCompilePhase :: Nil
    }
      projectPhases += ModuleCompilePhase(m, phase)

    if (phase == TestCompilePhase)
      projectPhases += ModuleCompilePhase(module, SourceCompilePhase)
    projectPhases.toSet
  }
   
    
  def upstreamProjectPhases = strictlyUpstreamProjectPhases + this

  def lastCompilationTime : Option[Long] = {
    if (compilationCacheFile.exists)
      lastModifiedProperFileTime(Set(compilationCacheFile))
    else
      None
  }

  def lastSourceModifcationTime : Option[Long] = lastModifiedProperFileTime(sourceFiles)

  private def changedFiles_(files : Iterable[File]) = {
    lastCompilationTime match {
      case Some(time) => files.filter(_.lastModified > time)
      case None => files
    }
  }

  def javaFilesChangedSinceLastCompilation = {
    changedFiles_(javaFiles).toSet
  }

  def sourceFilesDeletedSinceLastCompilation : Iterable[File] = {
    Option(module.analyses.get(outputDir)) match {
      case None => Set.empty
      case Some(analysis) => 
        analysis.infos.allInfos.keySet.filterNot(_.exists)
    }
  }

  def classpathDirectoriesAndJars : Iterable[File] = {
    upstreamProjectPhases.flatMap{
      pp => 
        pp.module.classpathJars.toSet + pp.resourceDir + pp.outputDir + pp.managedResourceDir
    }
  }
  def classpathJars = classpathDirectoriesAndJars.filter(_.isJar)
  def compilationClasspath = Module.asClasspathStr(classpathDirectoriesAndJars)

  val phaseDirectory = file(module.makerDirectory, phase.name).makeDirs()
  val compilationCacheFile = {
    file(phaseDirectory, "compilation-analysis-cache")
  }
  private val compilationFailedMarker = file(phaseDirectory, "compilation-failed-marker")
  def lastCompilationFailed() = compilationFailedMarker.exists
  def markCompilatonFailure() = compilationFailedMarker.touch

  def compilerLogger = {
    val lgr = ConsoleLogger(compilationOutputStream)
    lgr
  }

  def moduleCompilationErrorsFile = {
    phase match {
      case TestCompilePhase =>  file(module.rootAbsoluteFile, "module-vim-test-compile-errors")
      case SourceCompilePhase => file(module.rootAbsoluteFile, "module-vim-compile-errors")
    }
  }
  def compilationOutputStream : PrintStream = {
    val outputStream = if (module.isTestProject){
      new NullOutputStream
    } else {
      new TeeOutputStream(
        Console.err,
        new FileOutputStream(moduleCompilationErrorsFile)
      )
    }
    new PrintStream(outputStream)
  }

}
