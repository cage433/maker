package starling.browser.service.internal

import scala.collection.Set
import scala.collection.mutable.HashMap
import scala.{Iterator, Some}

object HeterogeneousMap {
  def empty[K[_]] = new HeterogeneousMap[K]
}

class HeterogeneousMap[K[_]] extends Serializable {
  val underlying = new HashMap[K[_], Any]

  /** Compute the number of key-to-value mappings.
   *
   *  @return the number of mappings
   */
  def size: Int = underlying.size

  /** Check if this map maps <code>key</code> to a value and return the
   *  value if it exists.
   *
   *  @param  key the key of the mapping of interest
   *  @return     the value of the mapping, if it exists
   */
  def get[T](key: K[T]): Option[T] = underlying.get(key) match {
    case Some(v) => Some(v.asInstanceOf[T]) // Cast is unavoidable, but safe
    case None => None
  }

  /** Check if this map maps <code>key</code> to a value.
    *  Return that value if it exists, otherwise return <code>default</code>.
    */
  def getOrElse[T](key: K[T], default: => T): T =
    underlying.getOrElse(key, default).asInstanceOf[T]


  /** Is this an empty map?
   *
   *  @return <code>true</code> iff the map is empty.
   */
  def isEmpty : Boolean = underlying.isEmpty

  /** Retrieve the value which is associated with the given key. This
   *  method throws an exception if there is no mapping from the given
   *  key to a value.
   *
   *  @param  key the key
   *  @return     the value associated with the given key.
   */
  def apply[T](key: K[T]): T = get(key) match {
    case None => default(key)
    case Some(value) => value
  }

  /** Is the given key mapped to a value by this map?
   *
   *  @param key the key
   *  @return    <code>true</code> iff there is a mapping for key in this map
   */
  def contains[T](key: K[T]): Boolean = underlying.contains(key)

  /** Does this map contain a mapping from the given key to a value?
   *
   *  @param key the key
   *  @return    <code>true</code> iff there is a mapping for key in this map
   */
  def isDefinedAt[T](key: K[T]) = contains(key)

  /** Creates an iterator for all keys.
   *
   *  @return an iterator over all keys.
   */
  def keys: Iterator[K[_]] = underlying.keysIterator

  /** @return the keys of this map as a set.
   */
  def keySet: Set[K[_]] = underlying.keySet

  def addAll(other:HeterogeneousMap[K]) {
    other.underlying.foreach { case(key,value) => underlying(key) = value }
  }

  /** Creates an iterator for a contained values.
   *
   *  @return an iterator over all values.
   */
  def values: Iterator[Any] = underlying.valuesIterator

  /** The default value for the map, returned when a key is not found
   *  The method implemented here yields an error,
   *  but it might be overridden in subclasses.
   *
   *  @param key the given key value
   *  @throws Predef.NoSuchElementException
   */
  def default[T](key: K[T]): T =
    throw new NoSuchElementException("key not found: " + key)

  /** This method allows one to add a new mapping from <code>key</code>
   *  to <code>value</code> to the map. If the map already contains a
   *  mapping for <code>key</code>, it will be overridden by this
   *  function.
   *
   * @param key    The key to update
   * @param value  The new value
   */
  def update[T](key: K[T], value: T) = underlying.update(key, value)

  /** Add a key/value pair to this map.
   *  @param    kv the key/value pair.
   */
  def += [T](kv: (K[T], T)) { update(kv._1, kv._2) }

  /** Add a key/value pair to this map.
   *  @param    kv the key/value pair.
   *  @return   The map itself with the new binding added in place.
   */
  def + [T](kv: (K[T], T)): HeterogeneousMap[K] = { this += kv; this }

  /** Remove a key from this map, noop if key is not present.
   *  @param    key the key to be removed
   */
  def -= [T](key: K[T]) = underlying -= key

  /** Remove a key from this map
   *  @param    key the key to be removed
   *  @return   The map itself with the binding for <code>key</code> removed if
   *            it existed.
   */
  def - [T](key: K[T]): HeterogeneousMap[K] = { this -= key; this }

  /** Remove <code>key</code> from this map and return the element
   *  that the key was previously mapped to (if any).
   */
  def removeKey[T](key: K[T]): Option[T] =
    underlying.remove(key).map(_.asInstanceOf[T])

  /** Map <code>key</code> to <code>elem</code> in this map and return the element
   *  that the key was previously mapped to (if any).
   */
  def put[T](key: K[T], elem: T): Option[T] =
    underlying.put(key, elem).map(_.asInstanceOf[T])

  /** Removes all mappings from the map. After this operation is
   *  completed, the map is empty.
   */
  def clear() = underlying.clear

  /** Check if this map maps <code>key</code> to a value.
    * Return that value if it exists, otherwise put <code>default</code>
    * as that key's value and return it.
    */
  def getOrElseUpdate[T](key: K[T], default: => T): T =
    underlying.getOrElseUpdate(key, default).asInstanceOf[T]

  /** Compares two maps structurally; i.e. checks if all mappings
   *  contained in this map are also contained in the other map,
   *  and vice versa.
   *
   *  @param that the other map
   *  @return     <code>true</code> iff both maps contain exactly the
   *              same mappings.
   */
  override def equals(that: Any): Boolean = underlying.equals(that)

  /** A hash method compatible with <code>equals</code>
   */
  override def hashCode() = underlying.hashCode

  /** Creates a string representation for this map.
   *
   *  @return    a string showing all mappings
   */
  override def toString() = underlying.toString
}
