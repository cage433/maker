package maker.task.compile

import com.typesafe.zinc._
import xsbti.compile.CompileOrder
import java.io.File
import maker.utils.FileUtils._
import org.scalatest.Failed
import maker.utils.Int
import maker.ConfigPimps
import maker.project.{Module, ProjectTrait}

object ZincCompile extends ConfigPimps{

  lazy val zinc = new ZincClient()
  def apply(rootProject : ProjectTrait, module : Module, phase : CompilePhase, majorScalaVersion : String) : Int = {
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
          (m.classDirectory(majorScalaVersion, p) -> m.compilationCacheFile(majorScalaVersion, p))
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

    val extraJarArgs : List[String] = config.scalaVersion.scalaReflectJar.toList.flatMap{
      jar => 
        List("-scala-extra", jar.getAbsolutePath)
    }(scala.collection.breakOut)
    val arguments = List[String](
      "-log-level",
      "warn",
      "-no-color",
      "-java-home",
      config.javaHome.getCanonicalFile.getAbsolutePath,
      "-scala-compiler",
      config.scalaVersion.scalaCompilerJar.getAbsolutePath,
      "-scala-library",
      config.scalaVersion.scalaLibraryJar.getAbsolutePath,
      "-classpath",
      if (phase == SourceCompilePhase) rootProject.compilationClasspath(majorScalaVersion) else rootProject.testCompilationClasspath(majorScalaVersion),
      "-d",
      module.classDirectory(majorScalaVersion, phase).getAbsolutePath,
      "-compile-order",
      CompileOrder.Mixed.toString,
      "-analysis-cache",
      module.compilationCacheFile(majorScalaVersion, phase).getAbsolutePath
    ) ::: extraJarArgs :::
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
        rootProject.compilationOutputStream(phase),
        rootProject.compilationOutputStream(phase)
      )
      val analysis = Compiler.analysis(module.compilationCacheFile(majorScalaVersion, phase))
      module.analyses.put(module.classDirectory(majorScalaVersion, phase), analysis)
      result
    } catch {
      case e : Throwable => 
        println("Debug: Module: bad things")
        println(settings)
        throw e
    }

  
  }

}
