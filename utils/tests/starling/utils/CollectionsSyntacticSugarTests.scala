package starling.utils

import org.scalatest.testng.TestNGSuite
import org.testng.annotations.Test
import org.testng.Assert._

class CollectionsSyntacticSugarTests extends TestNGSuite with CollectionsSyntacticSugar {
  @Test
  def testBackslashRemovesAnElementAndResultHasCorrectType{
    
    val x = List(1, 2, 3, 2)
    val y : List[Int] = x \ 2  // result type should be List[Int]
    assertEquals(y, List(1, 3))
  }

  @Test
  def testBackslashRemovalResultTypeMatchesObjectNotReference{

    val x : Seq[Int] = List(1, 2, 3, 2)
    val y  = x \ 2  // result type is a Seq[Int] reference to a List[Int] object
    assertEquals(y, List(1, 3))
  }

  @Test
  def testBackslashRemovalOfObjectNotInCollection{

    val x = List(1, 2, 3, 2)
    assertEquals(x \ "fred", x)
  }

  @Test
  def testBackslashBackslashRemoval{
    val x = List(1, 2, 3, 3, 2, 3, 4, 5)
    val q = scala.collection.immutable.Queue[Int](1, 3, 5)
    assertEquals(x \\ q, List(2, 2, 4))
  }
}
