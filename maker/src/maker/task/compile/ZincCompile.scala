package maker.task.compile

import com.typesafe.zinc._
import xsbti.compile.CompileOrder
import java.io.File
import maker.utils.FileUtils._

object ZincCompile{

  lazy val zinc = new ZincClient()
  def apply(projectPhase : ModuleCompilePhase) : Int = {
    val props = projectPhase.module.props
    val upstreamProjectPhases = projectPhase.strictlyUpstreamProjectPhases
    var upstreamCaches = Map[File, File]()
    upstreamProjectPhases.foreach{
      case pp : ModuleCompilePhase =>
        upstreamCaches += (pp.outputDir -> pp.compilationCacheFile)
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
      "error",
      "-no-color",
      "-scala-compiler",
      props.ProjectScalaCompilerJar().getAbsolutePath,
      "-scala-library",
      props.ProjectScalaLibraryJar().getAbsolutePath,
      "-scala-extra",
      props.ProjectScalaReflectJar().getAbsolutePath + File.pathSeparator + props.ProjectScalaXmlJar().getAbsolutePath,
      "-classpath",
      projectPhase.classpathDirectoriesAndJars.filterNot(_ == projectPhase.outputDir).toList.map(_.getCanonicalFile.getAbsolutePath).mkString(File.pathSeparator),
      "-d",
      projectPhase.outputDir.getAbsolutePath,
      "-compile-order",
      CompileOrder.Mixed.toString,
      "-analysis-cache",
      projectPhase.compilationCacheFile.getAbsolutePath
    ) :::
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
