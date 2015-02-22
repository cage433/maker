package maker.task.compile

import com.typesafe.zinc._
import xsbti.compile.CompileOrder
import java.io.File
import maker.utils.FileUtils._

object ZincCompile{

  lazy val zinc = new ZincClient()
  def apply(projectPhase : ModuleCompilePhase) : Int = {
    val module = projectPhase.module
    val props = module.props
    val upstreamCaches = {
      var map = Map[File, File]()

      projectPhase.strictlyUpstreamProjectPhases.foreach{
        case ModuleCompilePhase(module, phase) => 
          map += 
            (module.outputDir(phase) -> module.compilationCacheFile(phase))
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

    // TODO - fix this hack
    val extraJarArgs = if (module.scalaVersion.split('.')(1).toInt == 9)
      Nil
    else 
      List("-scala-extra", module.scalaReflectJar.getAbsolutePath)
    print(module.scalaReflectJar.getAbsolutePath)
    val arguments = List[String](
      "-log-level",
      "warn",
      "-no-color",
      "-java-home",
      props.JavaHome().getCanonicalFile.getAbsolutePath,
      "-scala-compiler",
      module.scalaCompilerJar.getAbsolutePath,
      "-scala-library",
      module.scalaLibraryJar.getAbsolutePath,
      "-classpath",
      projectPhase.classpathDirectoriesAndJars.filterNot(_ == projectPhase.outputDir).toList.map(_.getCanonicalFile.getAbsolutePath).mkString(File.pathSeparator),
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
