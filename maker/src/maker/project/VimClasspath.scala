package maker.project

import maker.task.compile._
import java.io.File
import maker.utils.FileUtils

object VimClasspath extends FileUtils {
  /**
    * To run tests from Vim it is convenient to have _all_ test classes on the classpath,
    * Not just those modules on whom we have a test dependency
    */
  def writeVimClasspath(project: ProjectTrait) {
    var components = CompilePhase.PHASES.flatMap{phase => 
      project.compilationTargetDirectories(phase :: Nil)
    }.distinct ++: project.resourceDirectories(TestCompilePhase :: SourceCompilePhase :: Nil)  // FreeTRM wants test reference.conf to 
                                                                                               // override main - however may want to change this for 
                                                                                               // other projects
    components ++= project.dependencyJars(TestCompilePhase)
    components ++= project.unmanagedLibs 
    components = components.filter{
      file =>
        file.exists && (!file.isDirectory || file.list.size > 0)
    }
    var relativePaths = components.map(_.relativeTo(file("."))).map(_.getPath)
    relativePaths = relativePaths.map{
      path => 
        if (path.endsWith(".jar")) {
          val pathComponents = path.split(File.separator)
          (pathComponents.dropRight(1) :+ "*").mkString(File.separator)
        } else 
          path
    }.distinct
    val cp = relativePaths.mkString("", File.pathSeparator, File.pathSeparator + sys.props("line.separator"))

    val cpFile : File = file("maker-classpath.txt")
    println(s"Writing classpath to file $cpFile")
    writeToFile(cpFile, cp)
  }
}
