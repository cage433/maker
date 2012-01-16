package starling.utils.conversions

import collection.mutable.ListBuffer

import starling.utils.ImplicitConversions._
import scalaz.Scalaz._


trait RichList {
  implicit def enrichList[A](list : List[A]) = new RichList(list)
  implicit def enrichListOfOptions[A](list : List[Option[A]]) = new RichList(list) {
    lazy val somes: List[A] = list.flatMap(_.toList)
  }

  class RichList[A](list : List[A]) {
    def castValues[T](error : Any => T)(implicit m : Manifest[T]) : List[T] = m.castAll(list, error)
    def filterCast[T](implicit m : Manifest[T]): List[T] = list.flatMap(m.cast(_))

    def allSame[B](f : A => B) : Boolean = if (list.length < 2) true else {
      val first = f(list(0))
      list.forall(elem => f(elem) == first)
    }

    def zipApply[B](fs : Seq[A => B]) : List[B] = list.zip(fs).map( fi => fi._2(fi._1))
    def containsDuplicates = list.toSet.size < list.size

    def duplicates: List[A] = {
      list.groupBy(id => id).filter { case (key, values) => values.size > 1 }.mapValues(_.head).values.toList
    }

    def &(other: List[A]) = list.toSet & other.toSet
    def toOption[T](value: => T) = (!list.isEmpty).option(value)

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

    def transform[A1 <: A](pf: PartialFunction[A, A1]): List[A] = list.map(pf.asFunction)

    def uniqueElement(errorMsg : String = "Expected unique element") = list.distinct match {
      case elt :: Nil => elt
      case Nil => throw new Exception(errorMsg + " - list was empty") 
      case _ => throw new Exception(errorMsg + " - list had more than one element " + list.take(2) + " ....") 
    }
  }
}
