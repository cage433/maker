package maker.task.compile

import com.typesafe.zinc._
import xsbti.compile.CompileOrder
import java.io.File
import maker.utils.FileUtils._
import org.scalatest.Failed
import maker.utils.Int
import maker.{ScalaVersion, Log}
import maker.project.{Module, ProjectTrait}

object ZincCompile extends Log {

  lazy val zinc = new ZincClient()

  def apply(rootProject : ProjectTrait, module : Module, phase : CompilePhase) : Int = {
    val upstreamCaches = {
      var map = Map[File, File]()

      val strictlyUpstreamModules = module.upstreamModules.filterNot(_ == module)
      val phases = if (phase == SourceCompilePhase) 
        Vector(SourceCompilePhase) 
      else
        Vector(SourceCompilePhase, phase) 

      for {
        m <- strictlyUpstreamModules;
        p <- phases
      }{
        map += 
          (m.classDirectory(p) -> m.compilationCacheFile(p))
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

    val arguments = Seq[String](
      "-log-level",
      "warn",
      "-transactional",
      "-no-color",
      "-java-home",
      rootProject.javaHome.getCanonicalFile.getAbsolutePath,
      "-scala-compiler",
      rootProject.scalaCompilerJar.getAbsolutePath,
      "-scala-library",
      rootProject.scalaLibraryJar.getAbsolutePath,
      "-classpath",
      module.compilationClasspath(phase),
      "-d",
      module.classDirectory(phase).getAbsolutePath,
      "-compile-order",
      CompileOrder.Mixed.toString,
      "-analysis-cache",
      module.compilationCacheFile(phase).getAbsolutePath,
      "-scala-extra",
      rootProject.scalaReflectJar.getAbsolutePath
    ) ++:
    module.scalacOptions.map("-S" +_) ++:
    module.javacOptions.map("-C" +_) ++:
    analysisMapArguments ++: 
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
        module.compilationOutputStream(phase),
        module.compilationOutputStream(phase)
      )
      val analysis = Compiler.analysis(module.compilationCacheFile(phase))
      rootProject.analyses.put(module.classDirectory(phase), analysis)
      result
    } catch {
      case e : Throwable => 
        println("Debug: Module: bad things")
        println(settings)
        throw e
    }

  
  }

}

