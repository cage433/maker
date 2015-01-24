package maker.task.compile

import maker.utils.RichString._

trait CompilationState
case object CompilationSucceeded extends CompilationState
case object CompilationNotRequired extends CompilationState

case class CompilationFailed(error : String) extends CompilationState
