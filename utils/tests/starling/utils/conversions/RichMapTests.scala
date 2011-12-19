package starling.utils.conversions

import org.scalatest.matchers.ShouldMatchers
import collection.immutable.Map
import starling.utils.BidirectionalHashMap
import java.util.concurrent.atomic.AtomicInteger
import org.scalatest.WordSpec
import scalaz.Scalaz._


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
    Map(1 → "foo", 2 → "bar") >>> Map("foo" → 3, "baz" → 4) should be === Map(1 → 3)
    Map(1 → "foo", 2 → "bar") innerJoin Map("foo" → 3, "baz" → 4) should be === Map(1 → 3)
  }

  "can flatMapKeys" in {
    Map(1 → "foo", 2 → "bar") flatMapKeys(Map(1 → 10)) should be === Map(10 → "foo")
    Map(1 → "foo", 2 → "bar") flatMapKeys(value => (value == 2).option(20)) should be === Map(20 → "bar")
  }
}

class RichMultiMapTests extends WordSpec with ShouldMatchers with RichMaps with RichAnys {
  "can form union of two multimaps" in {
    Map(1 ->> (1, 2, 3), 2 ->> (2, 3, 4)) union Map(1 ->> (4, 5, 6), 3 ->> (3, 4, 5)) should be ===
      Map(1 ->> (1, 2, 3, 4, 5, 6), 2 ->> (2, 3, 4), 3 ->> (3, 4, 5))
  }

  "can reverse" in {
    Map(1 → "ABC".toList, 2 → "DEF".toList).reverseMultiMap should be === Map('A' → 1, 'B' → 1, 'C' → 1, 'D' → 2, 'E' → 2, 'F' → 2)
  }
}


class BidirectionalMapTests extends WordSpec with ShouldMatchers {
  "should only execute update function once" in {
    val (bimap, counter) = create

    bimap.getOrElseUpdate(1, counter.incrementAndGet().toString)

    bimap(1) should be === "1"
    bimap.inverse("1") should be === 1
  }

  "should add to inverse" in {
    val (bimap, _) = create

    bimap += (1 → "one")

    bimap(1) should be === "one"
    bimap.inverse("one") should be === 1
  }

  def create = (BidirectionalHashMap.empty[Int, String], new AtomicInteger(0))
}