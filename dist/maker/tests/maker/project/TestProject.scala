package maker.project

import java.io.File
import maker.utils.FileUtils._
import maker.MakerProps
import java.util.concurrent.ConcurrentHashMap
import sbt.inc.Analysis
import maker.task.compile._

class TestProject(
  root : File, 
  name : String,
  upstreamProjects : List[Project] = Nil, 
  upstreamTestProjects : List[Project] = Nil, 
  props : MakerProps = TestProject.makeTestProps(MakerProps()),
  analyses :ConcurrentHashMap[File, Analysis] = new ConcurrentHashMap[File, Analysis]()
) extends Project(
  root, 
  name,
  new MakerProjectLayout(root){override def unmanagedLibDirs = Set(file("utils/lib_managed"))}, 
  upstreamProjects, 
  upstreamTestProjects, 
  props,
  None,
  Dependencies.Null,
  analyses
){
  def writeSrc(relativeSrcPath : String, code : String, phase : CompilePhase = SourceCompilePhase) = {
    val dir = phase match {
      case SourceCompilePhase ⇒ sourceDirectory
      case TestCompilePhase ⇒ testDirectory
    }
    writeToFile(file(dir, relativeSrcPath), code.stripMargin)
  }
  def writeTest(relativeSrcPath : String, code : String) = writeSrc(relativeSrcPath, code, TestCompilePhase)
}

object TestProject{
  def makeTestProps(props : MakerProps) : MakerProps = {
    MakerProps(
      "MakerLogLevel", "ERROR", 
      "ShowCompilerOutput", "false", 
      "ShowTestProgress", "false", 
      "StripInfoFromTaskResults", "false",
      "MakerHome", props.MakerHome(),
      "ScalaCompilerJar", props.ScalaCompilerJar().getPath,
      "ScalaLibraryJar", props.ScalaLibraryJar().getPath,
      "SbtInterfaceJar", props.SbtInterfaceJar().getPath,
      "CompilerInterfaceSourcesJar", props.CompilerInterfaceSourcesJar().getPath
    )
  }
}
