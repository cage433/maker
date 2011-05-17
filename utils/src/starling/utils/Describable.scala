package starling.utils

case class Describable[T](value : T, message : String)

object Describable {
  def require(requirement : Describable[Boolean]) = Predef.require(requirement.value, requirement.message)
}
