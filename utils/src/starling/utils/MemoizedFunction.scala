package starling.utils


/** Attaches a cache to a function to avoid recalculation
 */
case class MemoizedFunction[A, B](fn : A => B){

  private var cache : Map[A, B] = Map.empty[A, B]

  def apply(a : A) : B = {
    if (! cache.contains(a))
      cache += a -> fn(a)
    cache(a)
  }

  /** Return the size of the cache - Currently just used in unit tests to check the thing is working.
   *
   */
  def size = cache.size
}
/** Attaches a cache to a function to avoid recalculation
 */
case class MemoizedTwoArgFunction[A, B, C](fn : (A, B) => C){

  private var cache : Map[(A, B), C] = Map.empty[(A, B), C]

  def apply(a : A, b : B) : C = {
    if (! cache.contains((a, b)))
      cache += (a, b) -> fn(a, b)
    cache((a, b))
  }

  /** Return the size of the cache - Currently just used in unit tests to check the thing is working.
   *
   */
  def size = cache.size
}

object MemoizedFunction{
  def memoize[A, B](fn: A => B) = MemoizedFunction(fn)
  def memoize2[A, B, C](fn: (A, B) => C) = MemoizedTwoArgFunction(fn)
}
