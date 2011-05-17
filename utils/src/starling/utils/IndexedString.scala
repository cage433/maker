package starling.utils


case class IndexedString(value: String, index: Int) extends Ordered[IndexedString] {
  def compare(that: IndexedString) = index.compare(that.index) match {
    case 0 => value.compare(that.value)
    case other => other
  }

  override def toString = value.toString
}

