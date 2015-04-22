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
  def apply(rootProject : ProjectTrait, projectPhase : ModuleCompilePhase) : Int = {
    val module = projectPhase.module
    val phase = projectPhase.phase
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
          (m.outputDir(p) -> m.compilationCacheFile(p))
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
      rootProject.classpath(phase),
      "-d",
      projectPhase.outputDir.getAbsolutePath,
      "-compile-order",
      CompileOrder.Mixed.toString,
      "-analysis-cache",
      projectPhase.compilationCacheFile.getAbsolutePath
    ) ::: extraJarArgs :::
    projectPhase.module.scalacOptions.map("-S" +_) :::
    projectPhase.module.javacOptions.map("-C" +_) :::
    analysisMapArguments ::: 
      projectPhase.sourceFiles.toList.map(_.getAbsolutePath)

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
      val result = zinc.run(arguments, projectPhase.module.rootAbsoluteFile, projectPhase.compilationOutputStream, projectPhase.compilationOutputStream)
      val analysis = Compiler.analysis(projectPhase.compilationCacheFile)
      projectPhase.module.analyses.put(projectPhase.outputDir, analysis)
      result
    } catch {
      case e : Throwable => 
        println("Debug: Module: bad things")
        println(settings)
        throw e
    }

  
  }

}
