package maker.project

import maker.task.compile._
import java.io.File
import maker.utils.FileUtils

trait ClassicLayout extends FileUtils {
  this: Module =>
  override def sourceDirs(compilePhase : CompilePhase) : List[File] = {
    ClassicLayout.sourceDirs(rootAbsoluteFile, compilePhase)
  }

  override def resourceDir(compilePhase : CompilePhase) = {
    ClassicLayout.resourceDir(rootAbsoluteFile, compilePhase)
  }
}

object ClassicLayout extends FileUtils {
  def sourceDirs(rootDirectory: File, compilePhase : CompilePhase) : List[File] = compilePhase match {
    case SourceCompilePhase           => file(rootDirectory, "src") :: Nil
    case TestCompilePhase             => file(rootDirectory, "tests") :: Nil
    case IntegrationTestCompilePhase  => file(rootDirectory, "it") :: Nil
    case EndToEndTestCompilePhase     => file(rootDirectory, "e2e") :: Nil
  }
  def resourceDir(rootDirectory: File, compilePhase : CompilePhase) = compilePhase match {
    case SourceCompilePhase           => file(rootDirectory, "resources")
    case TestCompilePhase             => file(rootDirectory, "test-resources")
    case IntegrationTestCompilePhase  => file(rootDirectory, "integration-test-resources")
    case EndToEndTestCompilePhase     => file(rootDirectory, "e2e-test-resources")
  }

}
