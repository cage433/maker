package maker.task.compile

import java.io.File
import maker.task.Dependency
import maker.utils.FileUtils._
import java.io.BufferedWriter
import maker.project.Project
import sbt.ConsoleLogger
import java.io.PrintStream
import org.apache.commons.io.output.TeeOutputStream
import org.apache.commons.io.output.NullOutputStream
import java.io.FileOutputStream
import com.typesafe.zinc.Compiler

case class ProjectPhase(project : Project, phase : CompilePhase){
  import ProjectPhase._
  val log = project.log

  def sourceDirs = phase match {
    case SourceCompilePhase ⇒ project.layout.sourceDirs
    case TestCompilePhase ⇒ project.layout.testSourceDirs
  }

  def outputDir : File = {
    val f = phase match {
      case SourceCompilePhase ⇒ project.layout.outputDir
      case TestCompilePhase ⇒ project.layout.testOutputDir
    }
    f.mkdirs
    f
  }


  def resourceDirs : Set[File] = {
    val ds = phase match {
      case SourceCompilePhase ⇒ project.layout.resourceDirs
      case TestCompilePhase ⇒ project.layout.testResourceDirs
    }
    ds.foreach(_.mkdirs)
    ds
  }

  def scalaFiles = findFilesWithExtension("scala", sourceDirs)
  def javaFiles = findFilesWithExtension("java", sourceDirs)

  def sourceFiles = scalaFiles ++ javaFiles
  def classFiles : Set[File] = {
    findClasses(outputDir)
  }
  def classNames : Set[String] = {
    // used for class dependency analysis
    classFiles.map(_.relativeTo(outputDir)).map(_.getPath).filterNot(_.contains("$$")).map(_.replace('/', '.').dropRight(6)) 
  }

  def strictlyUpstreamProjectPhases = {
    val task = CompileTask(project, phase)
    Dependency.Graph.transitiveClosure(task).nodes.filterNot(_== task).collect{
      case ct : CompileTask ⇒ ct.projectPhase
    }
  }
  def upstreamProjectPhases = strictlyUpstreamProjectPhases + this
  def fullyQualifiedClassesOnly : Set[String] = {
    classFiles.map(_.className(outputDir))
  }
  def fullyQualifiedClasses : Set[String] = upstreamProjectPhases.toSet.flatMap{pp : ProjectPhase ⇒ pp.classFiles.map(_.className(pp.outputDir))}

  def lastCompilationTime : Option[Long] = {
    if (compilationCacheFile.exists)
      lastModifiedProperFileTime(Set(compilationCacheFile))
    else
      None
  }

  def lastSourceModifcationTime : Option[Long] = lastModifiedProperFileTime(sourceFiles)

  private def changedFiles_(files : Set[File]) = {
    lastCompilationTime match {
      case Some(time) => files.filter(_.lastModified > time)
      case None => files
    }
  }

  def javaFilesChangedSinceLastCompilation = {
    changedFiles_(javaFiles).toSet
  }

  def sourceFilesDeletedSinceLastCompilation : Set[File] = {
    Option(project.analyses.get(outputDir)) match {
      case None ⇒ Set.empty
      case Some(analysis) ⇒ 
        analysis.infos.allInfos.keySet.filterNot(_.exists)
    }
  }

  def classpathDirectoriesAndJars : Set[File] = {
    upstreamProjectPhases.flatMap{
      pp ⇒ 
        pp.resourceDirs ++ pp.project.classpathJarsOnly + pp.outputDir 
    }
  }
  def classpathJars = classpathDirectoriesAndJars.filter(_.isJar)
  def compilationClasspath = Project.asClasspathStr(classpathDirectoriesAndJars)

  val makerDirectory = mkdirs(file(project.rootAbsoluteFile, ".maker", phase.name))
  val compilationCacheFile = {
    file(makerDirectory, "compilation-analysis-cache")
  }

  def compilerLogger = {
    val lgr = ConsoleLogger(compilationOutputStream)
      //lgr.setLevel(sbt.Level.Debug)
    lgr
  }

  def vimCompileOutputFile = {
    phase match {
      case TestCompilePhase ⇒  file(project.rootAbsoluteFile, "project-vim-compile-output")
      case SourceCompilePhase ⇒ file(project.rootAbsoluteFile, "project-vim-test-compile-output") 
    }
  }
  private def compilationOutputStream : PrintStream = {
    val outputStream = if (project.props.ShowCompilerOutput()){
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
    val jars = project.allUpstreamProjects.flatMap(_.classpathJars).toSet.toList
    val metadataFiles : List[File] = {
      val compileTask = phase match {
        case SourceCompilePhase ⇒ SourceCompileTask(project)
        case TestCompilePhase ⇒ TestCompileTask(project)
      }
      Dependency.Graph.transitiveClosure(compileTask).nodes.filterNot(_ == compileTask).flatMap{
        case ct : CompileTask ⇒  Some(ct.projectPhase.compilationCacheFile)
        case _ ⇒  None
      }.toList
    }
    (src ::: jars ::: metadataFiles.toList).filter(_.exists)
   }

   def upstreamCacheMaps = strictlyUpstreamProjectPhases.map{
     pp ⇒ 
      pp.outputDir → pp.compilationCacheFile
    }.toMap

}

