package starling.models

trait ExerciseType

object American extends ExerciseType {
  override def toString = "American"
}
object European extends ExerciseType {
  override def toString = "European"
}

object ExerciseType {
  def unapply(s: String): Option[ExerciseType] = {
    if(s.toLowerCase.startsWith("amer")) {
      Some(American)
    } else if(s.toLowerCase.startsWith("eur")) {
      Some(European)
    } else {
      None
    }
  }
}
