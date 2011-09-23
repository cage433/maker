package starling.quantity

/**
 * Methods for constructing arbitrary lists of primes. 
 */
object Primes{
  import Stream.cons

 	private def ints(n : Int) : Stream[Int] = cons(n, ints(n + 1))
 	private def primes(nums : Stream[Int]) : Stream[Int] =
    cons(
    		nums.head, 
    		primes(nums.tail.filter(_ % nums.head != 0))
    )
  private def primes() : Stream[Int] = primes(ints(2))
  
  def firstNPrimes(n : Int) : List[Int] = {
    val primeStream = primes
    (0 until n).toList.map(primeStream)
  }

  private def createStream(s: Stream[Int]): Stream[Int] =
    Stream.cons(s.head, createStream(s.tail filter {
      _ % s.head != 0
    }))

  def primeStream: Stream[Int] = createStream(Stream.from(2))

  def primeStreamIterator(from: Int) = {
    val it = primeStream.toIterator
    while(it.next < from) {}
    it
  }

  def isPrime(num: Int): Boolean = {
    val primes = primeStream
    (num > 1) && (primes takeWhile {
      _ <= math.sqrt(num)
    } forall {
      num % _ != 0
    })
  }
}
