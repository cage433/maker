package starling.utils

import collection.immutable.SortedMap

/**
 * A very partial implementation of Map, intended to replace code such as
 *  map1 ++ map2
 * so as to improve performance (yes I've measured) and memory usage. The only method
 * supported is 'get'. Others could be implemented but would be prohibitively slow.
 * For example an implementation of 'iterator' might be
 *
    def iterator = {
      var alreadyUsed : Set[A] = Set()
      maps.toList match {
        case first::rest => {
          alreadyUsed ++= first.keys
          (first.iterator /: rest.map(_.iterator)){
            (l, r ) =>
              val r2 = r.filterNot{case (k, v) => alreadyUsed.contains(k)}
              alreadyUsed ++= r2.map(_._1)
              l ++ r2
          }
        }
        case _ => Map[A, B]().iterator
      }
    }
 *
 * but the performance sucks so badly an exception is thrown instead.
 *
 * I've also left out '+' and '-'. It's possible a fast implementation of these
 * could be implemented but it's not needed for the moment.
 *
 */

class AppendingMap[A, B](val namedMaps : Map[String,Map[A, B]]) extends scala.collection.immutable.Map[A, B]{
  def -(key: A) = throw new UnsupportedOperationException()

  def +[B1 >: B](kv: (A, B1)) = throw new UnsupportedOperationException()

  def add(name:String, other:Map[A,B]) = new AppendingMap(namedMaps + (name -> other))

  val maps: List[Map[A, B]] = namedMaps.values.toList

  def iterator = new Iterator[(A,B)] {
    val mapsIterator = {
      maps.iterator
    }
    var currentIterator:Iterator[(A,B)] = null
    def hasNext = {
      while((currentIterator == null || !currentIterator.hasNext) && mapsIterator.hasNext) {
        currentIterator = mapsIterator.next.iterator
      }
      currentIterator != null && currentIterator.hasNext
    }
    def next() = {
      currentIterator.next
    }
  }

  override def keySet = Set() ++ maps.flatMap(_.keySet)


  /**
   * Note that
   *    AppendingMap(map1, map2)
   * behaves differently to
   *    map1 ++ map2
   *
   * If both contain 'key' then in the former case map1's value is returned, in the latter it is map2's
   */

  def get(key: A) = maps.find(_.contains(key)) match {
    case Some(map) => map.get(key)
    case None => None
  }
}
