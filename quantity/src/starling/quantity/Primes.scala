package starling.quantity

/**
 * Methods for constructing arbitrary lists of primes. 
 */
object Primes {

  class Gen {
    private def merge(xs: Stream[Int], ys: Stream[Int]): Stream[Int] = {
      if (xs.head < ys.head)
        return Stream.cons(xs.head, merge(xs.tail, ys))
      if (xs.head == ys.head)
        return Stream.cons(xs.head, merge(xs.tail, ys.tail))
      return Stream.cons(ys.head, merge(xs, ys.tail))
    }

    private def diff(xs: Stream[Int], ys: Stream[Int]): Stream[Int] = {
      if (xs.head < ys.head)
        return Stream.cons(xs.head, diff(xs.tail, ys))
      if (xs.head == ys.head) return diff(xs.tail, ys.tail)
      return diff(xs, ys.tail)
    }

    private def f(xs: Stream[Int], ys: => Stream[Int]) =
      Stream.cons(xs.head, merge(xs.tail, ys))

    private def g(p: Int) = Stream.from(p * p, p * 2)

    private def foldr1(f: (Stream[Int], => Stream[Int]) => Stream[Int])
                      (s: Stream[Stream[Int]]): Stream[Int] =
      if (s.isEmpty) Stream.empty
      else f(s.head, foldr1(f)(s.tail))

    val primes: Stream[Int] =
      ((2 :: 3 :: 5 :: Nil).toStream).append(
        diff(Stream.from(7, 2), nonprimes))
    val nonprimes = foldr1(f)(primes.tail map g)
  }

  def primeStream: Stream[Int] = new Gen().primes

  def primeStreamIterator = primeStream.toIterator

  def isPrime(num: Int): Boolean = {
    val primes = primeStream
    (num > 1) && (primes takeWhile {
      _ <= math.sqrt(num)
    } forall {
      num % _ != 0
    })
  }
}
