package starling.utils.conversions


trait RichBoolean {
  implicit def booleanToRichBoolean(input : Boolean) = new RichBoolean(input)

  class RichBoolean(input : Boolean) {
    def toOption[T](value : => T) : Option[T] = if (input) Some(value) else None
  }
}