package starling.quantity

import org.scalatest.testng.TestNGSuite
import org.testng.Assert._
import org.testng.annotations.Test
import starling.quantity.UOM._

class PrimesTests extends TestNGSuite {
  @Test
  def testPrimes {

    val stream1 = Primes.primeStreamIterator
    val stream2 = Primes.primeStreamIterator

    (0 to 100).map {
      i => assertEquals(stream1.next(), stream2.next())
    }
  }

  @Test
  def testFirstPrimes1 {
    val primes = Primes.primeStream.take(9)
    assertEquals(List(2, 3, 5, 7, 11, 13, 17, 19, 23), primes)
  }

  @Test
  def testFirstPrimes2 {
    val primes = Primes.primeStream.take(100)
    primes.map(p => assertTrue(Primes.isPrime(p)))
  }

}
