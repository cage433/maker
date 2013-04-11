package maker.task.compile

import com.typesafe.zinc._
import xsbti.compile.CompileOrder
import java.io.File
import maker.utils.FileUtils._

object ZincCompile{

  lazy val zinc = new ZincClient()
  def apply(projectPhase : ProjectPhase) : Int = {
    val props = projectPhase.project.props
    val upstreamProjectPhases = projectPhase.strictlyUpstreamProjectPhases
    var upstreamCaches = Map[File, File]()
    upstreamProjectPhases.foreach{
      case pp : ProjectPhase ⇒ 
        upstreamCaches += (pp.outputDir → pp.compilationCacheFile)
    }

    val analysisMapArguments = upstreamCaches.filter{
      case (_, f) ⇒ f.exists
    }.map{
      case (key, value) ⇒ key.getAbsolutePath + File.pathSeparator + value.getAbsolutePath
    }.toList match {
      case Nil ⇒ Nil
      case list ⇒ List("-analysis-map", list.mkString(","))
    }

    val arguments = List[String](
      "-log-level",
      "info",
      "-scala-compiler",
      props.ScalaCompilerJar().getAbsolutePath(),
      "-scala-library",
      props.ScalaLibraryJar().getAbsolutePath(),
      //"-scala-extra",
      "-classpath",
      projectPhase.classpathDirectoriesAndJars.filterNot(_ == projectPhase.outputDir).toList.map(_.getCanonicalFile.getAbsolutePath).mkString(File.pathSeparator),
      "-d",
      projectPhase.outputDir.getAbsolutePath(),
      "-compile-order",
      CompileOrder.Mixed.toString,
      "-analysis-cache",
      projectPhase.compilationCacheFile.getAbsolutePath()
    ) :::
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
      case sf ⇒ 
        assert(sf.isScalaFile || sf.isJavaFile, "file " + sf + " in source files")
    }
    if (errors.size > 0){
      println(settings)
      errors.foreach(println)
      throw new RuntimeException("Failed to process compiler arguments")
    }


    try {
      zinc.run(arguments, projectPhase.project.rootAbsoluteFile, System.out, System.err);
    } catch {
      case e : Throwable ⇒ 
        println("Debug: Project: bad things")
        println(settings)
        throw e
    }

  
  }
}
