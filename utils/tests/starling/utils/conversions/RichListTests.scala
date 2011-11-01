package starling.utils.conversions

import org.scalatest.testng.TestNGSuite
import org.scalatest.matchers.ShouldMatchers
import org.testng.annotations.Test
import starling.utils.ImplicitConversions._
import xml.{Node, XML, Utility}

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

  @Test def shouldProduceNestedHtml {
    trimXML(List(1,2,3).mkHtml()) should be === <ul><li>1</li><li>2</li><li>3</li></ul>
  }

  @Test def x {
    val universe = Set(1,2,3,4,5)
    val a = Set(1,2,3)
  }

  private def trimXML(s: String): Node = Utility.trim(XML.loadString(s))
}