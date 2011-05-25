package starling.utils


object StringToInt {
  def unapply(s: String): Option[Int] = try {
    Some(s.toInt)
  } catch {
    case e: NumberFormatException => None
  }
}
