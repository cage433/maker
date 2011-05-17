package starling.utils.conversions

import org.scalatest.testng.TestNGSuite
import org.scalatest.matchers.ShouldMatchers
import org.testng.annotations.Test
import starling.utils.ImplicitConversions._


class RichListTests extends TestNGSuite with ShouldMatchers {
  @Test def duplicates {
    val nil: List[Int] = Nil
    nil.duplicates should be === nil
    List(1,2,3).duplicates should be === Nil
    List(1,1,1).duplicates should be === List(1)
    List(1,2,1).duplicates should be === List(1)
  }

  @Test def shouldBeAbleToBuildAListOutOfEitherElementsOrOtherListsInArbitraryOrder {
    val e = 3
    val l = List(1,2)
    val ll = List(l)

    e ::- e should be === e :: e :: Nil
    e ::- l should be === e :: l
    l ::- e should be === l ::: List(e)
    l ::- l should be === l ::: l

    e ::- l ::- l ::- e should be === e :: l ::: l ::: List(e)
    l ::- e ::- l ::- l ::- e ::- e should be === l ::: List(e) ::: l ::: l ::: List(e) ::: List(e)
  }
}