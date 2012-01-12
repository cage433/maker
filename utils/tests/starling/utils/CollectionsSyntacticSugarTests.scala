package starling.utils

import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers
import java.awt.Color._


class CollectionsSyntacticSugarTests extends WordSpec with ShouldMatchers with CollectionsSyntacticSugar {
  "\\ removes an element and result has correct type" in {
    val x = List(1, 2, 3, 2)
    val y : List[Int] = x \ 2  // result type should be List[Int]

    y should be === List(1, 3)
  }

  "\\ removal result type matches object not reference" in {
    val x : Seq[Int] = List(1, 2, 3, 2)
    val y  = x \ 2  // result type is a Seq[Int] reference to a List[Int] object

    y should be === List(1, 3)
  }

  "\\ removal of object not in collection" in {
    val x = List(1, 2, 3, 2)

    (x \ "fred") should be === x
  }

  "\\\\ removal" in {
    val x = List(1, 2, 3, 3, 2, 3, 4, 5)
    val q = scala.collection.immutable.Queue[Int](1, 3, 5)

    (x \\ q) should be === List(2, 2, 4)
  }

  "can convert TraversableLike[(A (B C))] to NestedMap[A, B, C] (i.e. Map[A, Map[B, C])" in {
    List((1, ("foo", 1.2)), (2, ("bar", 2.3)), (1, ("food", 3.4)), (2, ("barf", 4.5))).toNestedMap should be ===
      Map(1 → Map("foo" → 1.2, "food" → 3.4), 2 → Map("bar" → 2.3, "barf" → 4.5))
  }

  "can convert TraversableLike[(A (B (C D)))] to NestedMap3[A, B, C, D] (i.e. Map[A, Map[B, Map[C, D]]])" in {
    List((1, ("foo",  (1.2, RED))),    (2, ("bar",  (2.3, GREEN))),
         (1, ("foo",  (2.1, CYAN))),   (2, ("bar",  (3.2, MAGENTA))),
         (1, ("food", (3.4, BLUE))),   (2, ("barf", (4.5, WHITE))),
         (1, ("food", (4.3, YELLOW))), (2, ("barf", (5.4, BLACK)))).toNestedMap3 should be ===
      Map(1 → Map("foo" → Map(1.2 → RED, 2.1 → CYAN), "food" → Map(3.4 → BLUE, 4.3 → YELLOW)),
          2 → Map("bar" → Map(2.3 → GREEN, 3.2 → MAGENTA), "barf" → Map(4.5 → WHITE, 5.4 → BLACK)))
  }
}
