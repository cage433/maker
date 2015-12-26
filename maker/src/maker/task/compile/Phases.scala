package maker.task.compile


trait Phase

sealed trait CompilePhase extends Phase {
  def name : String
}

case object SourceCompilePhase extends CompilePhase {
  def name = "source-compile"
  override def toString = "Source compile"
}

sealed trait TestPhase extends CompilePhase

case object TestCompilePhase extends TestPhase {
  def name = "test-compile"
  override def toString = "Test compile"
}

case object IntegrationTestCompilePhase extends TestPhase {
  def name = "integration-test-compile"
  override def toString = "Integration test compile"
}

case object EndToEndTestCompilePhase extends TestPhase {
  def name = "e2e-test-compile"
  override def toString = "End-to-end test compile"
}

object CompilePhase{
  def apply(name : String) : CompilePhase = {
    List(SourceCompilePhase, TestCompilePhase, IntegrationTestCompilePhase, EndToEndTestCompilePhase).find(
      _.name == name
    ) match {
      case Some(phase) => phase
      case None => 
        throw new RuntimeException("Unrecognised phase name " + name)
    }
  }

  val TEST_PHASES = Vector(TestCompilePhase, IntegrationTestCompilePhase, EndToEndTestCompilePhase)
  val PHASES = SourceCompilePhase +: TEST_PHASES
}
case object Run extends Phase
