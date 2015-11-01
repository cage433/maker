package maker.project

import maker.task.compile._
import java.io.File
import maker.utils.FileUtils

trait ClassicLayout extends FileUtils {
  this: Module =>
  override def sourceDirs(compilePhase : CompilePhase) : List[File] = compilePhase match {
    case SourceCompilePhase           => file(rootAbsoluteFile, "src") :: Nil
    case TestCompilePhase             => file(rootAbsoluteFile, "tests") :: Nil
    case IntegrationTestCompilePhase  => file(rootAbsoluteFile, "it") :: Nil
    case EndToEndTestCompilePhase     => file(rootAbsoluteFile, "e2e") :: Nil
  }
  override def resourceDir(compilePhase : CompilePhase) = compilePhase match {
    case SourceCompilePhase           => file(rootAbsoluteFile, "resources")
    case TestCompilePhase             => file(rootAbsoluteFile, "test-resources")
    case IntegrationTestCompilePhase  => file(rootAbsoluteFile, "integration-test-resources")
    case EndToEndTestCompilePhase     => file(rootAbsoluteFile, "e2e-test-resources")
  }
}

