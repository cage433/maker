package starling.utils

/**
 * Handy when you want to use a case-insenstive string as member variable of a case class or as an
 * index in a map.
 *
 * No need to create this manually as the implicit defs in the companion object will just convert
 * it from a String when needed.
 *
 * Warning: CaseInsensitive("ASDF") == "asdf" but "asdf" != CaseInsensitive("ASDF") 
 */
case class CaseInsensitive(s: String) extends Ordered[CaseInsensitive] {
  val self: String = s.toLowerCase

  override def equals(p1: Any) = p1 match {
    case o: CaseInsensitive => self == o.self
    case s: String => self == s.toLowerCase
    case _ => s == p1
  }

  override def hashCode = self.hashCode

  def compare(other: CaseInsensitive) = self compareTo other.self

  override def toString = s

  /**
   * ignore case - used for implicits. e.g. "asdfs".i will give you a case
   * insensitive string
   */
  def i = this
}

object CaseInsensitive {
  implicit def sensitize(c: CaseInsensitive) = c.s

  implicit def desensitize(s: String) = CaseInsensitive(s)
}
