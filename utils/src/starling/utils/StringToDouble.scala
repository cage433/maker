package starling.utils


object StringToDouble {
  def unapply(s: String): Option[Double] = try {
    Some(s.toDouble)
  } catch {
    case e: NumberFormatException => None
  }
}
