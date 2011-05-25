package starling.utils


object Int {
  def unapply(s: String): Option[Int] = try {
    Some(s.toInt)
  } catch {
    case _: java.lang.NumberFormatException => None
  }
}

object Double {
  def unapply(s: String): Option[Double] = try {
    Some(s.toDouble)
  } catch {
    case _: java.lang.NumberFormatException => None
  }
}