package maker.task.compile

import java.io.File
import maker.task.Dependency
import maker.utils.FileUtils._
import maker.project.Module
import sbt.ConsoleLogger
import java.io.PrintStream
import org.apache.commons.io.output.TeeOutputStream
import org.apache.commons.io.output.NullOutputStream
import java.io.FileOutputStream

case class ModuleCompilePhase(module : Module, phase : CompilePhase){
  val log = module.log

  def sourceDirs = phase match {
    case SourceCompilePhase => module.sourceDirs
    case TestCompilePhase => module.testSourceDirs
  }

  def outputDir : File = {
    val f = phase match {
      case SourceCompilePhase => module.outputDir
      case TestCompilePhase => module.testOutputDir
    }
    f.mkdirs
    f
  }


  def resourceDir : File = {
    val d = phase match {
      case SourceCompilePhase => module.resourceDir
      case TestCompilePhase => module.testResourceDir
    }
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
    val task = CompileTask(module, phase)
    Dependency.Graph.transitiveClosure(module, task).nodes.filterNot(_== task).collect{
      case ct : CompileTask => ct.modulePhase
    }
  }
  def upstreamProjectPhases = strictlyUpstreamProjectPhases + this
  def fullyQualifiedClassesOnly : Iterable[String] = {
    classFiles.map(_.className(outputDir))
  }
  def fullyQualifiedClasses : Iterable[String] = upstreamProjectPhases.toSet.flatMap{pp : ModuleCompilePhase => pp.classFiles.map(_.className(pp.outputDir))}

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

  val phaseDirectory = mkdir(file(module.makerDirectory, phase.name))
  val compilationCacheFile = {
    file(phaseDirectory, "compilation-analysis-cache")
  }

  def compilerLogger = {
    val lgr = ConsoleLogger(compilationOutputStream)
    lgr
  }

  def vimCompileOutputFile = {
    phase match {
      case TestCompilePhase =>  file(module.rootAbsoluteFile, "module-vim-compile-output")
      case SourceCompilePhase => file(module.rootAbsoluteFile, "module-vim-test-compile-output")
    }
  }
  def compilationOutputStream : PrintStream = {
    val outputStream = if (module.props.ShowCompilerOutput()){
      new TeeOutputStream(
        Console.err,
        new FileOutputStream(vimCompileOutputFile)
      )
    } else {
      new NullOutputStream
    }
    new PrintStream(outputStream)
  }

  def compilationDependencies() = {
    val src = sourceFiles.toList
    val jars = module.allUpstreamModules.flatMap(_.classpathJars).toSet.toList
    val metadataFiles : List[File] = {
      val compileTask = phase match {
        case SourceCompilePhase => SourceCompileTask(module)
        case TestCompilePhase => TestCompileTask(module)
      }
      Dependency.Graph.transitiveClosure(module, compileTask).nodes.filterNot(_ == compileTask).flatMap{
        case ct : CompileTask =>  Some(ct.modulePhase.compilationCacheFile)
        case _ =>  None
      }.toList
    }
    (src ::: jars ::: metadataFiles.toList).filter(_.exists)
   }

   def upstreamCacheMaps = strictlyUpstreamProjectPhases.map{
     pp => 
      pp.outputDir -> pp.compilationCacheFile
    }.toMap

}
