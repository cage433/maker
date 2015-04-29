package maker.task.compile

import com.typesafe.zinc._
import xsbti.compile.CompileOrder
import java.io.File
import maker.utils.FileUtils._
import org.scalatest.Failed
import maker.utils.Int
import maker.{ConfigPimps, ScalaVersion}
import maker.project.{Module, ProjectTrait}

object ZincCompile extends ConfigPimps{

  lazy val zinc = new ZincClient()
  def apply(rootProject : ProjectTrait, module : Module, phase : CompilePhase, scalaVersion : ScalaVersion) : Int = {
    val config = module.config
    val upstreamCaches = {
      var map = Map[File, File]()

      val strictlyUpstreamModules = module.upstreamModules.filterNot(_ == module)
      val phases = if (phase == SourceCompilePhase) 
        Vector(SourceCompilePhase) 
      else
        Vector(SourceCompilePhase, TestCompilePhase) 

      for {
        m <- strictlyUpstreamModules;
        p <- phases
      }{
        map += 
          (m.classDirectory(scalaVersion, p) -> m.compilationCacheFile(scalaVersion, p))
      }

      map
    }
      
    val analysisMapArguments = upstreamCaches.filter{
      case (_, f) => f.exists
    }.map{
      case (key, value) => key.getAbsolutePath + File.pathSeparator + value.getAbsolutePath
    }.toList match {
      case Nil => Nil
      case list => List("-analysis-map", list.mkString(","))
    }

    val arguments = List[String](
      "-log-level",
      "warn",
      "-no-color",
      "-java-home",
      config.javaHome.getCanonicalFile.getAbsolutePath,
      "-scala-compiler",
      rootProject.scalaCompilerJar(scalaVersion).getAbsolutePath,
      "-scala-library",
      rootProject.scalaLibraryJar(scalaVersion).getAbsolutePath,
      "-classpath",
      if (phase == SourceCompilePhase) rootProject.compilationClasspath(scalaVersion) else rootProject.testCompilationClasspath(scalaVersion),
      "-d",
      module.classDirectory(scalaVersion, phase).getAbsolutePath,
      "-compile-order",
      CompileOrder.Mixed.toString,
      "-analysis-cache",
      module.compilationCacheFile(scalaVersion, phase).getAbsolutePath,
      "-scala-extra",
      rootProject.scalaReflectJar(scalaVersion).getAbsolutePath
    ) :::
    module.scalacOptions.map("-S" +_) :::
    module.javacOptions.map("-C" +_) :::
    analysisMapArguments ::: 
      module.sourceFiles(phase).toList.map(_.getAbsolutePath)

    val Parsed(rawSettings, residual, errors) = Settings.parse(
      arguments
    )

    // normalise relative paths to the current working directory (if provided)
    val settings = Settings.normalise(rawSettings, None)
    val inputs = Inputs(settings)
    val setup = Setup(settings)

    settings.sources.foreach{
      case sf => 
        assert(sf.isScalaFile || sf.isJavaFile, "file " + sf + " in source files")
    }
    if (errors.size > 0){
      println(settings)
      errors.foreach(println)
      throw new RuntimeException("Failed to process compiler arguments")
    }


    try {
      val result = zinc.run(
        arguments, 
        module.rootAbsoluteFile, 
        module.compilationOutputStream(scalaVersion, phase),
        module.compilationOutputStream(scalaVersion, phase)
      )
      val analysis = Compiler.analysis(module.compilationCacheFile(scalaVersion, phase))
      module.analyses.put(module.classDirectory(scalaVersion, phase), analysis)
      result
    } catch {
      case e : Throwable => 
        println("Debug: Module: bad things")
        println(settings)
        throw e
    }

  
  }

}
