package starling.utils.conversions

import starling.utils.ImplicitConversions
import collection.mutable.ListBuffer


trait RichList {
  implicit def enrichList[A](list : List[A]) = new RichList(list)
  implicit def enrichListOfOptions[A](list : List[Option[A]]) = new RichList(list) {
    lazy val somes = list.flatMap(_.toList)
  }

  class RichList[A](list : List[A]) {
    import ImplicitConversions._

    // TODO: Drop when we use scala 2.9
    def inits = list.reverse.tails.reverse.map(_.reverse)

    // TODO: Drop when we use scala 2.9
    def tails = {
      def recurse(list : List[A]) : List[List[A]] = list match {
        case Nil => Nil
        case _ => list :: recurse(list.tail)
      }

      recurse(list)
    }

    def castValues[T](error : Any => T)(implicit m : Manifest[T]) : List[T] = m.castAll(list, error)
    def filterCast[T](implicit m : Manifest[T]): List[T] = list.flatMap(m.cast(_))

    def asMap[B](keyF : A => B): Map[B, A] = list.map(i => (keyF(i), i)).toMap
    def valuesToMap[B](valueF: A => B): Map[A, B] = list.map(key => (key, valueF(key))).toMap

    def allSame[B](f : A => B) : Boolean = {
      if (list.length > 1) {
        val first = f(list(0))
        list.forall(elem => f(elem) == first)
      }
      else {
        true
      }
    }

    def zipApply[B](fs : Seq[A => B]) : List[B] = {
      list.zip(fs).map( fi => fi._2(fi._1))
    }

    def containsDuplicates = list.toSet.size < list.size
    def duplicates: List[A] = {
      list.groupBy(id => id).filter { case (key, values) => values.size > 1 }.mapValues(_.head).values.toList
    }

    def &(other: List[A]) = list.toSet & other.toSet

    def toOption[T](value: => T) = (!list.isEmpty).toOption(value)

    def flatMapO[B](f: A => Option[B]): List[B] = list.map(f).somes

    def ::-(elem: A): List[A] = list ::: List(elem)
    def ::-(other: List[A]): List[A] = list ::: other

    def indexesMatching(predicate: A => Boolean) = list.zipWithIndex.filter(indexed => predicate(indexed._1)).map(_._2)

    def elementsAt(indexes: List[Int]): List[A] = indexes.map(list(_))

    def split[That](predicate: A => Boolean, include: Boolean = true): List[List[A]] = {
      val results = new ListBuffer[List[A]]
      val current = new ListBuffer[A]

      list.foreach(item => {
        if (predicate(item)) {
          if (include) {
            current += item
          }
          results += current.toList
          current.clear
        } else {
          current += item
        }
      })

      if (!current.isEmpty) {
        results += current.toList
      }

      results.toList
    }
  }
}