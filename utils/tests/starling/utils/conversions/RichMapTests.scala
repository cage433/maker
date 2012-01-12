package starling.utils.conversions

import org.scalatest.testng.TestNGSuite
import org.scalatest.matchers.ShouldMatchers
import collection.immutable.Map
import starling.utils.BidirectionalHashMap
import java.util.concurrent.atomic.AtomicInteger
import org.testng.annotations.{BeforeMethod, Test}
import org.scalatest.WordSpec
import collection.mutable.{Map => MMap}
import java.lang.String


class RichMapTests extends WordSpec with ShouldMatchers with RichMaps {
  val nothing: Map[Int, String] = Map[Int, String]()
  val map = Map(1 → "2")
  val mapWithDifferentValue = Map(1 → "3")

  "a map minus itself equals nothing" in {
    map difference map should be === nothing
    nothing difference nothing should be === nothing
  }

  "a map minus nothing equals itself" in {
    map difference nothing should be === map
  }

  "nothing minus a map equals nothing" in {
    nothing difference map should be === nothing
  }

  "a map minus a map with a different value should equal itself" in {
    map difference mapWithDifferentValue should be === map
  }

  "can join two maps" in {
    Map(1 → "foo", 2 → "bar") innerJoin Map("foo" → 3, "baz" → 4) should be === Map(1 → 3)
  }
}

class RichMultiMapTests extends WordSpec with ShouldMatchers with RichMaps with RichAnys {
  "can form union of two multimaps" in {
    Map(1 ->> (1, 2, 3), 2 ->> (2, 3, 4)) union Map(1 ->> (4, 5, 6), 3 ->> (3, 4, 5)) should be ===
      Map(1 ->> (1, 2, 3, 4, 5, 6), 2 ->> (2, 3, 4), 3 ->> (3, 4, 5))
  }

  "can reverse multimap" in {
    Map(1 ->> (2, 3), 2 ->> (3, 4)).reverseMulti.mapValues(_.sorted) should be ===
      Map(2 ->> (1), 3 ->> (1, 2), 4 ->> (2))
  }

  "can flatten" in {
    Map(1 ->> (10, 20, 30), 2 ->> (40, 50, 60)).flatMultiMap(kv => kv._1 + kv._2).toList should be ===
      List(11, 21, 31, 42, 52, 62)
  }
}

class RichNestedMapTests extends WordSpec with ShouldMatchers with RichMaps {
  "can pair inner and outer keys" in  {
    Map(1 → Map(2 → 20, 3 → 30)).pairKeys should be === Map((1, 2) → 20, (1, 3) → 30)
  }

  "can map inner values" in {
    Map(1 → Map(2 → 20, 3 → 30)).mapInnerValues(_ * 2) should be ===
      Map(1 → Map(2 → 40, 3 → 60))
  }
}

class RichMutableMapTests extends WordSpec with ShouldMatchers with RichMaps {
  "insertOrUpdate should insert when the map is empty" in {
    val map = MMap.empty[Int, String]

    map.insertOrUpdate(1, "inserted", _ + " updated") should be === None
    map should be === Map(1 → "inserted").mutable
  }

  "insertOrUpdate should update when the map isn't empty" in {
    val map = Map(1 → "initial").mutable

    map.insertOrUpdate(1, "inserted", _ + " updated") should be === Some("initial")
    map should be === Map(1 → "initial updated").mutable
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