package starling.utils

/**
 * Scala objects don't implement readResolve so in the GUI matches fail when mixing local and deserialized objects
 * Extending this class should fix this
 */
trait StarlingObject {
  override def equals(obj: Any) = obj != null && this.getClass.getName == obj.asInstanceOf[AnyRef].getClass.getName
  override def hashCode = getClass.getName.hashCode
}