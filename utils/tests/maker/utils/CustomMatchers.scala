package maker.utils

import org.scalatest.matchers.{MatchResult, Matcher}

trait CustomMatchers{
  class SequenceContains[T](predicate : T => Boolean) extends Matcher[Seq[T]]{
    def apply(left : Seq[T]) = {
      MatchResult(
        left.exists(predicate),
        s"Sequence did not have an element passing predicate",
        s"Sequence did have an element passing predicate"
      )
    }
  }

  class SequenceContainsAll[T](predicate : T => Boolean) extends Matcher[Seq[T]]{
    def apply(left : Seq[T]) = {
      MatchResult(
        left.forall(predicate),
        s"Sequence did not have every element passing predicate",
        s"Sequence did have every element passing predicate"
      )
    }
  }
  def containElementSatisfying[T](predicate : T => Boolean) = new SequenceContains(predicate)
  def allSatisfy[T](predicate : T => Boolean) = new SequenceContainsAll(predicate)
}

object CustomMatchers extends CustomMatchers
