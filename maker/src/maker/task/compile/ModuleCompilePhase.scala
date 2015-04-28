package maker.task.compile

import maker.task.Dependency
import maker.utils.FileUtils._
import maker.project.Module
import sbt.ConsoleLogger
import org.apache.commons.io.output.{TeeOutputStream, NullOutputStream}
import ch.qos.logback.classic.Logger
import org.slf4j.LoggerFactory
import java.io.{File, FileOutputStream, PrintStream}
import maker.ScalaVersion

case class ModuleCompilePhase(module : Module, phase : CompilePhase){

  def sourceDirs = module.sourceDirs(phase)

  //def outputDir : File = {
    //val f = module.outputDir(phase)
    //f.mkdirs
    //f
  //}


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

  def scalaFiles = findFilesWithExtension("scala", sourceDirs : _*)
  def javaFiles = findFilesWithExtension("java", sourceDirs: _*)

  def sourceFiles = scalaFiles ++ javaFiles

  //def classNames : Seq[String] = {
    //// used for class dependency analysis
    //classFiles.map(_.relativeTo(outputDir)).map(_.getPath).filterNot(_.contains("$$")).map(_.replace('/', '.').dropRight(6)) 
  //}

  def lastCompilationTime : Option[Long] = {
    if (compilationCacheFile.exists)
      lastModifiedProperFileTime(Vector(compilationCacheFile))
    else
      None
  }

  def lastSourceModifcationTime : Option[Long] = lastModifiedProperFileTime(sourceFiles)

  private def changedFiles_(files : Seq[File]) = {
    lastCompilationTime match {
      case Some(time) => files.filter(_.lastModified > time)
      case None => files
    }
  }

  def javaFilesChangedSinceLastCompilation = {
    changedFiles_(javaFiles).toSet
  }

  def sourceFilesDeletedSinceLastCompilation(scalaVersion : ScalaVersion) : Seq[File] = {
    Option(module.analyses.get(module.classDirectory(scalaVersion, phase))) match {
      case None => Nil
      case Some(analysis) => 
        analysis.infos.allInfos.keySet.toVector.filterNot(_.exists)
    }
  }


  val phaseDirectory = file(module.makerDirectory, phase.name).makeDirs()
  val compilationCacheFile = {
    file(phaseDirectory, "compilation-analysis-cache")
  }
  private val compilationFailedMarker = file(phaseDirectory, "compilation-failed-marker")
  def lastCompilationFailed() = compilationFailedMarker.exists
  def markCompilatonFailure() = compilationFailedMarker.touch

  def moduleCompilationErrorsFile = {
    phase match {
      case TestCompilePhase =>  file(module.rootAbsoluteFile, "module-vim-test-compile-errors")
      case SourceCompilePhase => file(module.rootAbsoluteFile, "module-vim-compile-errors")
    }
  }

}
