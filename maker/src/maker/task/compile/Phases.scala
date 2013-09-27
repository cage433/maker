package maker.task.compile


trait Phase

trait CompilePhase extends Phase {
  def name : String
}

case object SourceCompilePhase extends CompilePhase {
  def name = "source-compile"
  override def toString = "Source compile"
}

case object TestCompilePhase extends CompilePhase {
  def name = "test-compile"
  override def toString = "Test compile"
}

object CompilePhase{
  def apply(name : String) : CompilePhase = {
    if (name == SourceCompilePhase.toString)
      SourceCompilePhase
    else if (name == TestCompilePhase.toString)
      TestCompilePhase
    else
      throw new RuntimeException("Unrecognised phase name " + name)
  }
}
case object Run extends Phase
