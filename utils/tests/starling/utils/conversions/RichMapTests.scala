package starling.utils.conversions

import org.scalatest.testng.TestNGSuite
import org.scalatest.matchers.ShouldMatchers
import collection.immutable.Map
import starling.utils.BidirectionalHashMap
import java.util.concurrent.atomic.AtomicInteger
import org.testng.annotations.{BeforeMethod, Test}


class RichMapTests extends TestNGSuite with ShouldMatchers with RichMaps {
  val nothing: Map[Int, String] = Map[Int, String]()
  val map = Map(1 → "2")
  val mapWithDifferentValue = Map(1 → "3")

  @Test def aMapMinusItselfEqualsNothing {
    map difference map should be === nothing
    nothing difference nothing should be === nothing
  }

  @Test def aMapMinusNothingEqualsItself {
    map difference nothing should be === map
  }

  @Test def nothingMinusAMapEqualsNothing {
    nothing difference map should be === nothing
  }

  @Test def aMapMinusAMapWithADifferentValueShouldEqualItself {
    map difference mapWithDifferentValue should be === map
  }

  @Test def canJoinTwoMaps {
    Map(1 → "foo", 2 → "bar") >>> Map("foo" → 3, "baz" → 4) should be === Map(1 → 3)
  }
}

class RichMultiMapTests extends TestNGSuite with ShouldMatchers with RichMaps with RichAnys {
  @Test def canFormUnionOfTwoMultiMaps {
    Map(1 ->> (1, 2, 3), 2 ->> (2, 3, 4)) union Map(1 ->> (4, 5, 6), 3 ->> (3, 4, 5)) should be ===
      Map(1 ->> (1, 2, 3, 4, 5, 6), 2 ->> (2, 3, 4), 3 ->> (3, 4, 5))
  }
}


class BidirectionalMapTests extends TestNGSuite with ShouldMatchers {
  val bimap = BidirectionalHashMap.empty[Int, String]
  val counter = new AtomicInteger(0)

  @BeforeMethod def setUp {
    bimap.clear
    counter.set(0)
  }

  @Test def shouldOnlyExecuteUpdateFunctionOnce {
    bimap.getOrElseUpdate(1, counter.incrementAndGet().toString)

    bimap(1) should be === "1"
    bimap.inverse("1") should be === 1
  }

  @Test def shouldAddToInverse {
    bimap += (1 → "one")

    bimap(1) should be === "one"
    bimap.inverse("one") should be === 1
  }
}