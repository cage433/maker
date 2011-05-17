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
}
