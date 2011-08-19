package starling.utils.conversions

import collection.mutable.ListBuffer

import starling.utils.ImplicitConversions._


trait RichList {
  implicit def enrichList[A](list : List[A]) = new RichList(list)
  implicit def enrichListOfOptions[A](list : List[Option[A]]) = new RichList(list) {
    lazy val somes = list.flatMap(_.toList)
  }
  implicit def enrichListOfTuple2[A, B](list: List[(A, B)]) = new RichList(list) {
    def mapFirst[C](f: A => C): List[(C, B)] = list.map(_.mapFirst(f))
    def mapSecond[C](f: B => C): List[(A, C)] = list.map(_.mapSecond(f))
    def _1 = list.map(_._1)
    def _2 = list.map(_._2)
  }

  class RichList[A](list : List[A]) {
    def castValues[T](error : Any => T)(implicit m : Manifest[T]) : List[T] = m.castAll(list, error)
    def filterCast[T](implicit m : Manifest[T]): List[T] = list.flatMap(m.safeCast(_))

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

    def partialMap[B](pf: PartialFunction[A, B]): List[B] = flatMapO(pf.lift)
    def safeMap[B](f: A => B): List[B] = flatMapO(f.option)
    def optMaxBy[B](f: A => B)(implicit cmp: Ordering[B]): Option[A] = if (list.isEmpty) None else Some(list.maxBy(f))
  }
}
